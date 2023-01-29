module SRC.Structure.Term(Term(..)) where

-- | Most important structure in program.
-- | Represents λ terms after input is processed by PARSER.
-- | Terms are represented internally using De Bruijn indexing (starting from 0).
data Term = 
    Var Int  |      -- Variable
    Abs Term |      -- Abstraction (λ. t)
    App Term Term   -- Application (t1 t2)
    deriving Eq

-- | Since we know that all variables will be small letters a-x,
-- we can use a fixed internal mapping for free variables in output:
-- a, b, c, ... = 0, 1, 2, ...
-- | Simply their position in latin alphabet.
-- | Bound variables in output will be outputed as letters prefixed with B.

instance Show Term where
    show = (showTerm 0)

-- | Converts λ terms to strings.
-- | First argument is depth in term representation tree. - "LAMBDA LEVEL"
-- | It's used to determine which varaiables are bound,
-- which are not and how to convert them.
-- 0 - (...)
-- 1 - λ.(...)
-- etc. ...
showTerm :: Int -> Term -> String
showTerm i (Var v)
    | v >= i  = (idx2char (v-i))        -- free
    | v < i = "B"++(idx2char (i-(v+1))) -- bound
showTerm i (Abs t)
    | i == 0 = r
    | i > 0  = "("++r++")"
    where r = "\\B"++(idx2char i)++"."++(showTerm (i+1) t)
showTerm i (App t1 t2)
    | i == 0 = r
    | i > 0  = "("++r++")"
    where r = (showTerm i t1)++" "++(showTerm i t2)

-- | Latin alphabet index -> Char
-- | NOTE: While free variables are bounded to a-z nobody says that bounded ones are too,
-- especialy, there is no limit on nesting terms.
-- | Following bounded variable naming scheme works as follows:
-- if x <= 25 -> name(x) in a-z 
-- else z+name(x-25)
idx2char :: Int -> String
idx2char n
    | n <= 25 = [toEnum (n+97)]
    | n > 25 = "z"++idx2char(n-26)