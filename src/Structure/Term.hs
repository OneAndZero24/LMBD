module Structure.Term(Term(..)) where

-- | Most important structure in program.
-- Represents λ terms after input is processed by PARSER.
-- | Terms are represented internally using De Bruijn indexing.
data Term = 
    Var Int |
    Abs Term |
    App Term Term
    deriving Eq

-- | Since we know that all variables will be small letters a-x,
-- we can use a fixed internal mapping for free variables in output:
-- a, b, c, ... = 0, 1, 2, ...
-- | Simply their position in latin alphabet.
-- | Bound variables in output will be outputed as letters prefixed with B.

instance Show Term where
    show = (showTerm 0)

-- | Converts λ terms to strings.
-- | Firstg argument is depth in term representation tree.
-- | It's used to determine which varaiables are bound,
-- which are not and how to convert them.
-- 0 - (...)
-- 1 - λ.(...)
-- etc. ...
showTerm :: Int -> Term -> String
showTerm i (Var v)
            | v > i  = [idx2char (v-i)] -- unbound
            | v <= i = [idx2char (i-v)] -- bound
showTerm i (Abs t) = "(\\B"++[(idx2char i)]++"."++(showTerm i t)++")"
showTerm i (App t1 t2) = "("++(showTerm (i+1) t1)++" "++(showTerm (i+1) t2)++")"

-- | Converts char to int. Using it's index in latin alphabet.
-- | a - 97 in ASCII table.
char2idx :: Char -> Int
char2idx = ((flip (-)) 97).fromEnum

-- | Does opposite to above.
idx2char :: Int -> Char
idx2char = toEnum.((+) 97)