import Data.List
import qualified Data.Map as M
import Data.Maybe
import Text.ParserCombinators.ReadP

data Ast = Lit Int | Var String | Lam [String] Ast | Let [(String, Ast)] Ast | App [Ast] | Spec String [Ast] deriving (Show, Eq, Ord)
data Program = Prog [(String, Ast)] Ast deriving (Show, Eq, Ord)

data Arg = Num Int | Lbl String deriving (Show, Eq, Ord)
data Op = Cmt String | Label String | Op String [Arg] deriving (Show, Eq, Ord)

keyws = ["def", "fun", "let"]

spec = ["+", "-", "*", "/", "=", ">", "<", ">=", "<=", "atom?", "cons", "car", "cdr", "if", "recur", "do"]

reserved = (`elem` (keyws ++ spec))

p_oneof = choice . (char `map`)

p_whitespace = p_oneof [' ', '\t', '\r', '\n']

p_ws = many1 p_whitespace

p_ows = optional p_ws

p_alpha = p_oneof (['A' .. 'Z'] ++ ['a' .. 'z'])

p_digit = p_oneof ['0' .. '9']

p_spec = p_oneof ['+', '-', '*', '/', '=', '<', '>']

p_alnum = choice [p_alpha, p_digit]

p_alspec = choice [p_alpha, p_spec]

p_idchar = choice [p_alnum, p_spec]

p_bw o c = (char o >> p_ows) `between` (p_ows >> char c)

p_bwpar = p_bw '(' ')'

p_bwbrk = p_bw '[' ']'

p_bwbrc = p_bw '{' '}'

p_par f = choice [p_bwpar f, p_bwbrk f, p_bwbrc f]

p_varp = p_par $ do
    id <- p_id
    p_ws
    expr <- p_expr
    return (id, expr)

p_anyid = do
    init <- p_alspec
    rest <- many p_idchar
    return (init : rest)

p_id = do
    id <- p_anyid
    if reserved id then pfail else return id

p_lit = do
    numstr <- many1 p_digit
    (return . Lit . read) numstr

p_var = do
    id <- p_id
    (return . Var) id

p_lam = p_par $ do
    string "fun"
    p_ws
    args <- p_par (p_id `sepBy` p_ws)
    p_ws
    expr <- p_expr
    return (Lam args expr)

p_let = p_par $ do
    string "let"
    p_ws
    vars <- p_par (p_varp `sepBy1` p_ws)
    p_ws
    expr <- p_expr
    p_ows
    return (Let vars expr)

p_app = do
    exprs <- p_par (p_expr `sepBy1` p_ws)
    (return . App) exprs

p_specf = p_par $ do
    name <- p_anyid
    if not $ name `elem` spec
        then pfail
        else do
            p_ws
            exprs <- p_expr `sepBy1` p_ws
            return (Spec name exprs)
    
p_expr = choice [p_lit, p_var, p_lam, p_let, p_app, p_specf]

p_def = p_par $ do
    string "def"
    p_ws
    id <- p_id
    p_ws
    expr <- p_expr
    return (id, expr)

p_program = do
    p_ows
    defs <- p_def `sepBy1` p_ws
    p_ws
    expr <- option (Lit 1) p_expr
    p_ows
    eof
    return (Prog defs expr)

var_lookup name (n, env) = [Num $ n - frame, Num ix]
    where
        (frame, ix) = fromJust $ name `M.lookup` env

--data Ast = Lit Int | Var String | Lam [String] Ast | Let [(String, Ast)] Ast | App [Ast] | Spec String [Ast] deriving (Show, Eq, Ord)
--spec = ["+", "-", "*", "/", "=", ">", "<", ">=", "<=", "atom?", "cons", "car", "cdr", "if", "recur", "do"]

cg n env labels (Lit x) = ([Op "LDC" [Num x]], labels)
cg n env labels (Var name) = ([Op "LD" (name `var_lookup` (n, env))], labels)
cg n env labels (Lam ids expr) = ([Op "LDF" [Lbl lbl]], (lbl, ((Cmt $ show expr) : inner_code) ++ [Op "RTN" []]) : inner_labels)
    where
        env' = foldl (\e (ix, name) -> M.insert name (n, ix) e) env ([0 ..] `zip` ids)
        (inner_code, inner_labels) = cg (succ n) env' labels expr
        lbl = "lambda_" ++ (show $ length inner_labels)
cg n env labels (Let defs expr) = ([Op "DUM" [Num $ length defs]] ++ code' ++ [Op "LDF" [Lbl lbl], Op "RAP" [Num $ length defs]], (lbl, inner_code ++ [Op "RTN" []]) : inner_labels)
    where
        env' = foldl (\e (ix, name) -> M.insert name (n, ix) e) env ([0 ..] `zip` (fst `map` defs))
        (code', labels') = foldl (\(c, l) expr -> let (c', l') = cg n env' l expr in (c ++ c', l')) ([], labels) (snd `map` defs)
        (inner_code, inner_labels) = cg (succ n) env' labels' expr
        lbl = "let_" ++ (show $ length inner_labels)
cg n env labels (App (f : rest)) = (code' ++ code'' ++ [Op "AP" [Num $ length rest]], labels'')
    where
        (code', labels') = foldl (\(c, l) expr -> let (c', l') = cg n env l expr in (c ++ c', l')) ([], labels) rest
        (code'', labels'') = cg n env labels' f
cg n env labels expr = ([Cmt $ "WARNING!!! Unable to generate code for {" ++ show expr ++ "}"], labels)

flatten acc [] = acc
flatten (line, labels, acc) (op@(Op _ _) : ops) = flatten (succ line, labels, op : acc) ops
flatten (line, labels, acc) ((Label lbl) : ops) = flatten (line, M.insert lbl line labels, (Cmt $ lbl ++ ":") : acc) ops
flatten (line, labels, acc) (op : ops) = flatten (line, labels, op : acc) ops

outarg (Num x) = show x
outarg (Lbl lbl) = lbl

out (Cmt str) = "; " ++ str
out (Label lbl) = lbl ++ ":"
out (Op name args) = "\t" ++ name ++ "\t" ++ (" " `intercalate` (outarg `map` args))

codegen (Prog defs expr) = "\n" `intercalate` (out `map` flat_code)
    where
        (code, labels) = cg (-1) M.empty [] (Let defs expr)
        (_, lbls, fc) = foldl (\acc (lbl, code) -> flatten acc ((Label lbl) : code)) (flatten (0, M.empty, []) code) (reverse labels)
        flat_code = reverse fc

main = do
    input <- getContents
    let ast = p_program `readP_to_S` input
    if length ast /= 1
        then putStrLn $ "No parse or multiple parses -- " ++ show (length ast) ++ " found."
        else putStrLn $ codegen (fst $ head ast)

