module Structure.Term(Term(..)) where --TODO

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
-- | Bound variables in output will be outputed as integers.

instance Show Lambda where
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
            | v > i  = idx2char (v-i) -- unbound
            | v <= i = show v         -- bound
showTerm i (Abs t) = "(\\"++(concatMap show (bound t))++(showTerm i+1 t)++")"
showTerm i (App t1 t2) = "("++(showTerm i+1 t1)++" "++(showTerm i+1 t2)++")"

-- | Converts char to int. Using it's index in latin alphabet.
-- | a - 97 in ASCII table.
char2idx :: Char -> Int
char2idx = fromEnum.((flip (-)) 97)

-- | Does opposite to above.
idx2char :: Int -> Char
idx2char = toEnum.((+) 97)

-- | Returns list of bound variables in term subtree.
bound :: Term -> [Int]
-- TODO

-- | Collects variables which fullfill given requirement.
collectVars :: Int -> (Term -> Int -> Bool) -> Term -> [Int]
-- TODO