import Data.List
import qualified Data.Map as M
import Data.Maybe
import System.IO
import Text.ParserCombinators.ReadP

import System.IO.Unsafe

data Ast = Lit Int | Var String | Lam [String] Ast | LamAbi [String] Ast | Let Bool [(String, Ast)] Ast | App [Ast] | Spec String [Ast] deriving (Show, Eq, Ord)
data Program = Prog [(String, Ast)] Ast deriving (Show, Eq, Ord)

data Arg = Num Int | Lbl String deriving (Show, Eq, Ord)
data Op = Cmt String | Label String | Op String [Arg] deriving (Show, Eq, Ord)

keyws = ["def", "fun", "fun-abi", "let", "let*"]

spec = ["+", "-", "*", "/", "=", ">", "<", ">=", "<=", "atom?", "cons", "car", "cdr", "if", "recur", "do", "debug", "!0", "!1", "set!"]

reserved = (`elem` (keyws ++ spec))

p_oneof = choice . (char `map`)

p_whitespace = p_oneof [' ', '\t', '\r', '\n']

p_comment = do
    char ';'
    many $ satisfy (/= '\n')
    char '\n'

p_wsc = choice [p_whitespace, p_comment]

p_ws = many1 p_wsc

p_ows = optional p_ws

p_alpha = p_oneof (['A' .. 'Z'] ++ ['a' .. 'z'])

p_digit = p_oneof ['0' .. '9']

p_spec = p_oneof ['+', '-', '*', '/', '=', '<', '>', '?', '!']

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
    if (not (null id) && head id == '-') || reserved id then pfail else return id

p_plit = do
    numstr <- many1 p_digit
    (return . Lit . read) numstr

p_nlit = do
    char '-'
    numstr <- many1 p_digit
    (return . Lit . (0 -) . read) numstr

p_lit = choice [p_plit, p_nlit]

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

p_lamabi = p_par $ do
    string "fun-abi"
    p_ws
    args <- p_par (p_id `sepBy` p_ws)
    p_ws
    expr <- p_expr
    return (LamAbi args expr)

p_let = p_par $ do
    string "let"
    flag <- option False (char '*' >> return True)
    p_ws
    vars <- p_par (p_varp `sepBy1` p_ws)
    p_ws
    expr <- p_expr
    p_ows
    return (Let flag vars expr)

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
    
p_expr = choice [p_lit, p_var, p_lam, p_lamabi, p_let, p_app, p_specf]

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
        (frame, ix) = (unsafePerformIO $ if name `M.member` env then return () else hPutStrLn stderr $ "Cannot find: " ++ name) `seq` (fromJust $ name `M.lookup` env)

cg_1 n env labels op a = (c_a ++ [Op op []], l)
    where
        (c_a, l) = cg n env labels a

cg_2 n env labels op a b = (c_a ++ c_b ++ [Op op []], l')
    where
        (c_a, l) = cg n env labels a
        (c_b, l') = cg n env l b

cg n env labels (Lit x) = ([Op "LDC" [Num x]], labels)
cg n env labels (Var name) = ([Op "LD" (name `var_lookup` (n, env))], labels)
-- ABI is not compatible with my approach to TCE
-- note that `fun-abi's MUST NOT be called internally
cg n env labels (LamAbi ids expr) = ([Op "LDF" [Lbl lbl]], (lbl, inner_code ++ [Op "RTN" []]) : inner_labels)
    where
        env' = foldl (\e (ix, name) -> M.insert name (succ n, ix) e) env ([0 ..] `zip` ids)
        (inner_code, inner_labels) = cg (succ n) env' labels expr
        lbl = "lambda_" ++ (show $ length inner_labels)
cg n env labels (Lam ids expr) = ([Op "LDF" [Lbl lbl]], (lbl, inner_code ++ [Op "RTN" []]) : inner_labels)
    where
        env' = foldl (\e (ix, name) -> M.insert name (succ n, ix) e) env ([0 ..] `zip` ("$@recur" : ids))
        (inner_code, inner_labels) = cg (succ n) env' labels expr
        lbl = "lambda_" ++ (show $ length inner_labels)
-- Mind the *
cg n env labels (Let False defs expr) = ([Op "DUM" [Num $ length defs]] ++ code' ++ [Op "LDF" [Lbl lbl], Op "RAP" [Num $ length defs]], (lbl, inner_code ++ [Op "RTN" []]) : inner_labels)
    where
        env' = foldl (\e (ix, name) -> M.insert name (succ n, ix) e) env ([0 ..] `zip` (fst `map` defs))
        (code', labels') = foldl (\(c, l) expr -> let (c', l') = cg (succ n) env' l expr in (c ++ c', l')) ([], labels) (snd `map` defs)
        (inner_code, inner_labels) = cg (succ n) env' labels' expr
        lbl = "let_" ++ (show $ length inner_labels)
cg n env labels (Let True [] expr) = cg n env labels expr
cg n env labels (Let True (def : defs) expr) = cg n env labels (Let False [def] (Let True defs expr))
cg n env labels (App all@(f : rest)) = (code' ++ code'' ++ [Op "AP" [Num $ succ $ length rest]], labels'')
    where
        (code', labels') = foldl (\(c, l) expr -> let (c', l') = cg n env l expr in (c ++ c', l')) ([], labels) all
        (code'', labels'') = cg n env labels' f
cg n env labels (Spec "+" [a, b]) = cg_2 n env labels "ADD" a b
cg n env labels (Spec "-" [a, b]) = cg_2 n env labels "SUB" a b
cg n env labels (Spec "*" [a, b]) = cg_2 n env labels "MUL" a b
cg n env labels (Spec "/" [a, b]) = cg_2 n env labels "DIV" a b
cg n env labels (Spec "=" [a, b]) = cg_2 n env labels "CEQ" a b
cg n env labels (Spec ">" [a, b]) = cg_2 n env labels "CGT" a b
cg n env labels (Spec "<" [a, b]) = cg_2 n env labels "CGT" b a -- funky!
cg n env labels (Spec ">=" [a, b]) = cg_2 n env labels "CGTE" a b
cg n env labels (Spec "<=" [a, b]) = cg_2 n env labels "CGTE" b a -- funky!
cg n env labels (Spec "atom?" [a]) = cg_1 n env labels "ATOM" a
cg n env labels (Spec "cons" [a, b]) = cg_2 n env labels "CONS" a b
cg n env labels (Spec "car" [a]) = cg_1 n env labels "CAR" a
cg n env labels (Spec "cdr" [a]) = cg_1 n env labels "CDR" a
-- if's MUST be in tail position!!!
cg n env labels (Spec "if" [cond, thn, els]) = (cond_code ++ [Op "TSEL" [Lbl t_lbl, Lbl e_lbl]], (e_lbl, els_code ++ [Op "RTN" []]) : labels''')
    where
        (cond_code, labels') = cg n env labels cond
        (thn_code, labels'') = cg n env labels' thn
        t_lbl = "then_" ++ (show $ length labels'')
        (els_code, labels''') = cg n env ((t_lbl, thn_code ++ [Op "RTN" []]) : labels'') els
        e_lbl = "else_" ++ (show $ length labels''')
-- Note that `recur' MUST ONLY be used with NAMED functions
-- In general, never apply to unnamed functions: might be expensive
cg n env labels (Spec "recur" args) = ([Op "LD" [Num 0, Num 0]] ++ code' ++ [Op "LD" [Num 0, Num 0], Op "TAP" [Num $ succ $ length args]], labels')
    where
        (code', labels') = foldl (\(c, l) expr -> let (c', l') = cg n env l expr in (c ++ c', l')) ([], labels) args
cg n env labels (Spec "do" []) = ([], labels)
cg n env labels (Spec "do" (expr : rest)) = (code' ++ code'', labels'')
    where
        (code', labels') = cg n env labels expr
        (code'', labels'') = cg n env labels' (Spec "do" rest)
cg n env labels (Spec "debug" [expr]) = (code' ++ [Op "DBUG" []], labels')
    where
        (code', labels') = cg n env labels expr
cg n env labels (Spec "!0" [Lit x]) = ([Op "LD" [Num x, Num 0]], labels)
cg n env labels (Spec "!1" [Lit x]) = ([Op "LD" [Num x, Num 1]], labels)
-- Unsupported: set!
cg n env labels expr = ([Cmt $ "WARNING!!! Unable to generate code for {" ++ show expr ++ "}"], labels)

flatten acc [] = acc
flatten (line, labels, acc) (op@(Op _ _) : ops) = flatten (succ line, labels, op : acc) ops
flatten (line, labels, acc) ((Label lbl) : ops) = flatten (line, M.insert lbl line labels, (Cmt $ lbl ++ ": (addr: " ++ show line ++ ")") : acc) ops
flatten (line, labels, acc) (op : ops) = flatten (line, labels, op : acc) ops

outarg _ (Num x) = show x
outarg lbls (Lbl lbl) = (unsafePerformIO $ if lbl `M.member` lbls then return () else hPutStrLn stderr $ "Cannot find: " ++ lbl) `seq` ((show . fromJust) $ lbl `M.lookup` lbls)

out _ (Cmt str) = "; " ++ str
out _ (Label lbl) = lbl ++ ":"
out lbls (Op name args) = "\t" ++ name ++ "\t" ++ (" " `intercalate` ((outarg lbls) `map` args))

codegen (Prog defs expr) = "\n" `intercalate` ((out lbls) `map` flat_code)
    where
        (code, labels) = cg (-1) M.empty [] (Let False defs expr)
        (_, lbls, fc) = foldl (\acc (lbl, code) -> flatten acc ((Label lbl) : code)) (flatten (0, M.empty, []) (code ++ [Op "RTN" []])) (reverse labels)
        flat_code = reverse fc

main = do
    hPutStrLn stderr "Scimitar compiler v0.0"
    input <- getContents
    let ast = p_program `readP_to_S` input
    if length ast /= 1
        then hPutStrLn stderr $ "No parse or multiple parses -- " ++ show (length ast) ++ " found."
        else do
            putStrLn $ codegen (fst $ head ast)
            hPutStrLn stderr "Task complete."
            hPutStrLn stderr ""

