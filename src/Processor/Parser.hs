module SRC.Processor.Parser(check, process) where

import SRC.Structure.Term
import SRC.Structure.Token

check :: [Token] -> Bool
check = (syntax 0 False)

-- | Basic syntax check
-- | Parameters: brackets level, is empty, token list
syntax :: Int -> Bool -> [Token] -> Bool
syntax _ _ [] = False                -- Last term has to be V or RB
syntax _ _ [(Lambda _)] = False      -- Lambda cannot be last term
syntax _ _ ((Lambda []):xs) = False  -- Lambda has to bind something
syntax i _ [(V c)] = (isaz c) && (i == 0)
syntax i e [RB] = (not e) && ((i-1) == 0)
syntax i _ ((V c):xs) = (isaz c) && (syntax i False xs)
syntax i _ ((Lambda l):xs) = syntax i False xs
syntax i _ (LB:xs) = syntax (i+1) True xs
syntax i e (RB:xs) = 
    if (ni >= 0)
        then (not e) && (syntax ni e xs)
        else False
    where ni = i-1

-- | Wrapper around parse
process :: [Token] -> Term
process = (parse 0 [] Nothing)

-- | Parses token list into Term tree,
-- also changes variables to De Bruijn representation.
-- | Parameters: Token List, Lambda level (as in term), 
-- Bound variables list - head is latest bound, Left accumulator
parse :: Int -> [Char] -> Maybe Term -> [Token] -> Term
parse i bv m [(V c)] = hl m (dbi c i bv)
parse i bv m ((V c):xs) = parse i bv (Just (hl m v)) xs
    where v = dbi c i bv
parse i bv m ((Lambda [l]):[x]) = hl m (Abs(parse (i+1) (l:bv) Nothing [x]))
parse i bv m ((Lambda [l]):(LB:xs)) = 
    if (length a) == 0
        then hl m p
        else parse i bv (Just(hl m p)) a
    where 
        (a, b) = select (xs, [])
        p = Abs(parse (i+1) (l:bv) Nothing b)
parse i bv m ((Lambda [l]):(x:xs)) = parse i bv (Just (hl m (Abs(parse (i+1) (l:bv) Nothing [x])))) xs
parse i bv m ((Lambda (l:ls)):[x]) = hl m (Abs(parse (i+1) (l:bv) Nothing ((Lambda ls):[x])))
parse i bv m ((Lambda (l:ls)):(LB:xs)) =
    if (length a) == 0
        then hl m p
        else parse i bv (Just(hl m p)) a
    where 
        (a, b) = select (xs, [])
        p = Abs(parse (i+1) (l:bv) Nothing ((Lambda ls):([LB]++b++[RB])))
parse i bv m ((Lambda (l:ls)):(x:xs)) = parse i bv (Just (hl m (Abs(parse (i+1) (l:bv) Nothing ((Lambda ls):[x]) )))) xs
parse i bv m ((LB):xs) =
    if (length a) == 0
        then hl m p
        else (parse i bv (Just (hl m p)) a)
    where 
        (a, b) = select (xs, [])
        p = parse i bv Nothing b

-- | Helper function, handles Maybe monad.
hl :: Maybe Term -> Term -> Term
hl Nothing t = t
hl (Just t1) t2 = App (t1) (t2)

-- | Selects until RB
select :: ([Token], [Token]) -> ([Token], [Token])
select ([], l) = ([], l)
select ((LB:as), l) = select (a, l++[LB]++b++[RB]) -- nested brackets
    where (a, b) = select (as, [])
select ((RB:as), l) = (as, l)
select ((a:as), l) = select (as, l++[a])   

-- | De Bruijn indexing for variable
dbi :: Char -> Int -> [Char] -> Term
dbi c i bv = 
    if bi == (length bv)
        then (Var ((char2idx c)+i)) -- free
        else (Var bi)               -- bound
    where bi = getBoundIdx c bv

-- | Retrieves variable from bound list
-- length - unbound otherwise lambda level
getBoundIdx :: Char -> [Char] -> Int
getBoundIdx _ [] = 0
getBoundIdx c (x:xs) = 
    if c == x
        then 0
        else 1+(getBoundIdx c xs)

-- | Converts char to int. Using it's index in latin alphabet.
-- | a - 97 in ASCII table.
-- If char below 97 or above 122 are requested they are mapped to closest in range.
char2idx :: Char -> Int
char2idx c 
    | n < 97 = 0
    | n >= 97 && n <= 122 = n-97
    | n > 122 = 25
    where n = fromEnum c