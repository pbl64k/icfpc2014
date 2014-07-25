import Text.ParserCombinators.ReadP

data Ast = Lit Int | Var String | Lam [String] Ast | Let [(String, Ast)] Ast | App [Ast] | Spec String [Ast] deriving (Show, Eq, Ord)
data Program = Prog [(String, Ast)] (Maybe Ast) deriving (Show, Eq, Ord)

keyws = ["def", "fun", "let"]

spec = ["+", "-", "*", "/", "=", "if", "do"]

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

p_bw o c = (char o) `between` (char c)

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
    p_ows
    return (Lam args expr)

p_let = p_par $ do
    string "let"
    p_ws
    vars <- p_par (between p_ows p_ows (p_varp `sepBy1` p_ws))
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
    expr <- option Nothing (p_expr >>= (return . Just))
    p_ows
    eof
    return (Prog defs expr)

main = do
    input <- getContents
    let ast = p_program `readP_to_S` input
    mapM_ (\(x, y) -> putStrLn $ show x) ast

