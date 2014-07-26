import Data.List
import qualified Data.Map as M
import Data.Maybe
import System.IO
import Text.ParserCombinators.ReadP

import System.IO.Unsafe

data Arg = Reg String | IReg String | Const Int | Addr Int | Lbl String deriving (Show, Eq, Ord)
data Op = Op (Maybe String) String [Arg] deriving (Show, Eq, Ord)

p_oneof = choice . (char `map`)

p_whitespace = p_oneof [' ', '\t', '\r']

p_ws = many1 p_whitespace

p_ows = optional p_ws

p_eol = char '\n'

p_comment = do
    char ';'
    many $ satisfy (/= '\n')
    return ()

p_indir = (char '[' >> p_ows) `between` (char ']' >> p_ows)

p_gpreg = do
    id <- p_oneof ['a' .. 'h']
    p_ows
    return (Reg [id])

p_igpreg = do
    (Reg id) <- p_indir p_gpreg
    return (IReg id)

p_pc = do
    id <- string "pc"
    p_ows
    return (Reg id)

p_ipc = do
    (Reg id) <- p_indir p_pc
    return (IReg id)

p_reg = choice [p_gpreg, p_pc]

p_ireg = choice [p_igpreg, p_ipc]

p_num = do
    num <- many1 $ p_oneof ['0' .. '9']
    let n = read num
    p_ows
    if n >= 0 && n <= 255
        then return (Const n)
        else pfail

p_addr = do
    (Const n) <- p_indir p_num
    return (Addr n)

p_lblid = many1 $ p_oneof $ concat [['a' .. 'z'], ['A' .. 'Z'], ['0' .. '9'], ['_']]

p_lblref = do
    char '$'
    id <- p_lblid
    p_ows
    return (Lbl id)

p_xlabel = do
    lbl <- p_lblid
    char ':'
    p_ows
    return lbl

p_label = choice [p_xlabel >>= (return . Just), return Nothing]

p_pcaddr = choice [p_num, p_lblref]

p_ramorreg = choice [p_reg, p_ireg, p_addr]

p_nonpcramorreg = choice [p_gpreg, p_igpreg, p_addr]

p_val = choice [p_ramorreg, p_num]

p_opmov = do
    lbl <- p_label
    string "mov"
    p_ws
    dest <- p_ramorreg
    p_ows
    char ','
    p_ows
    src <- p_val
    return (Op lbl "mov" [dest, src])

p_opinc = do
    lbl <- p_label
    string "inc"
    p_ws
    dest <- p_nonpcramorreg
    return (Op lbl "inc" [dest])

p_opdec = do
    lbl <- p_label
    string "dec"
    p_ws
    dest <- p_nonpcramorreg
    return (Op lbl "dec" [dest])

p_opadd = do
    lbl <- p_label
    string "add"
    p_ws
    dest <- p_nonpcramorreg
    p_ows
    char ','
    p_ows
    src <- p_val
    return (Op lbl "add" [dest, src])

p_opsub = do
    lbl <- p_label
    string "sub"
    p_ws
    dest <- p_nonpcramorreg
    p_ows
    char ','
    p_ows
    src <- p_val
    return (Op lbl "sub" [dest, src])

p_opmul = do
    lbl <- p_label
    string "mul"
    p_ws
    dest <- p_nonpcramorreg
    p_ows
    char ','
    p_ows
    src <- p_val
    return (Op lbl "mul" [dest, src])

p_opdiv = do
    lbl <- p_label
    string "div"
    p_ws
    dest <- p_nonpcramorreg
    p_ows
    char ','
    p_ows
    src <- p_val
    return (Op lbl "div" [dest, src])

p_opand = do
    lbl <- p_label
    string "and"
    p_ws
    dest <- p_nonpcramorreg
    p_ows
    char ','
    p_ows
    src <- p_val
    return (Op lbl "and" [dest, src])

p_opor = do
    lbl <- p_label
    string "or"
    p_ws
    dest <- p_nonpcramorreg
    p_ows
    char ','
    p_ows
    src <- p_val
    return (Op lbl "or" [dest, src])

p_opxor = do
    lbl <- p_label
    string "xor"
    p_ws
    dest <- p_nonpcramorreg
    p_ows
    char ','
    p_ows
    src <- p_val
    return (Op lbl "xor" [dest, src])

p_opjlt = do
    lbl <- p_label
    string "jlt"
    p_ws
    targ <- p_pcaddr
    p_ows
    char ','
    p_ows
    x <- p_val
    p_ows
    char ','
    p_ows
    y <- p_val
    return (Op lbl "jlt" [targ, x, y])

p_opjeq = do
    lbl <- p_label
    string "jeq"
    p_ws
    targ <- p_pcaddr
    p_ows
    char ','
    p_ows
    x <- p_val
    p_ows
    char ','
    p_ows
    y <- p_val
    return (Op lbl "jeq" [targ, x, y])

p_opjgt = do
    lbl <- p_label
    string "jgt"
    p_ws
    targ <- p_pcaddr
    p_ows
    char ','
    p_ows
    x <- p_val
    p_ows
    char ','
    p_ows
    y <- p_val
    return (Op lbl "jgt" [targ, x, y])

p_opint tag intnum = do
    lbl <- p_label
    string tag
    p_ows
    return (Op lbl "int" [Const intnum])

p_opset_dir = p_opint "set_dir" 0

p_opget_lm_1_pos = p_opint "get_lm_1_pos" 1

p_opget_lm_2_pos = p_opint "get_lm_2_pos" 2

p_opget_my_ix = p_opint "get_my_ix" 3

p_opget_gh_st_pos = p_opint "get_gh_st_pos" 4

p_opget_gh_cur_pos = p_opint "get_gh_cur_pos" 5

p_opget_gh_cur_st = p_opint "get_gh_cur_st" 6

p_opget_map_sq = p_opint "get_map_sq" 7

p_opdebug = p_opint "debug" 8

p_ophlt = do
    lbl <- p_label
    string "hlt"
    p_ows
    return (Op lbl "hlt" [])

p_op = choice [p_opmov, p_opinc, p_opdec, p_opadd, p_opsub, p_opmul, p_opdiv, p_opand, p_opor, p_opxor, p_opjlt, p_opjeq, p_opjgt,
    p_opset_dir, p_opget_lm_1_pos, p_opget_lm_2_pos, p_opget_my_ix, p_opget_gh_st_pos, p_opget_gh_cur_pos, p_opget_gh_cur_st,
    p_opget_map_sq, p_opdebug, p_ophlt]

p_linenop = do
    p_ows
    optional p_comment
    p_eol
    return []

p_lineop = do
    p_ows
    op <- p_op
    optional p_comment
    p_eol
    return [op]

p_program = do
    ops <- many $ choice [p_lineop, p_linenop]
    let opl = concat ops
    eof
    return opl

outlbl Nothing = ""
outlbl (Just lbl) = "\t; " ++ lbl ++ ":"

--data Arg = Reg String | IReg String | Const Int | Addr Int | Lbl String deriving (Show, Eq, Ord)

outarg _ (Reg n) = n
outarg lbls (IReg n) = "[" ++ outarg lbls (Reg n) ++ "]"
outarg _ (Const n) = show n
outarg lbls (Addr n) = "[" ++ outarg lbls (Const n) ++ "]"
outarg lbls (Lbl n) = show $ fromJust $ n `M.lookup` lbls

out lbls (Op lbl name args) = "\t" ++ name ++ "\t" ++ (", " `intercalate` ((outarg lbls) `map` args)) ++ outlbl lbl

codegen lines = "\n" `intercalate` ((out labels) `map` lines)
    where
        f m (_, (Op Nothing _ _)) = m
        f m (ix, (Op (Just lbl) _ _)) = M.insert lbl ix m
        labels = foldl f M.empty ([0 ..] `zip` lines)

main = do
    hPutStrLn stderr "GHC80 v0.0"
    input <- getContents
    let ast = p_program `readP_to_S` input
    if length ast /= 1
        then hPutStrLn stderr $ "No parse or multiple parses -- " ++ show (length ast) ++ " found."
        else
            if length (fst $ head ast) > 256
                then hPutStrLn stderr $ "Program too long -- " ++ show (length (fst $ head ast)) ++ " line(s)."
                else do
                    putStrLn $ codegen (fst $ head ast)
                    hPutStrLn stderr "Task complete."
                    hPutStrLn stderr ""

