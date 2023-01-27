module Processor.Parser(check, parse) where

import Structure.Term
import Structure.Token

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
syntax _ _ ((V c):xs) = (isaz c) && (syntax i False xs)
syntax _ _ ((Lambda l):xs) = syntax i False xs
syntax i _ (LB:xs) = syntax (i+1) True xs
syntax i e (RB:xs) = 
    if (ni >= 0)
        then (not e) && (syntax ni e xs)
        else False
    where ni = i-1

-- | Checks whether variable is in legal range
isaz :: Char -> Bool
isaz c = (n >= 97) && (n <= 122)
    where n = fromEnum c

-- | Parses token list into Term tree,
-- also changes variables to De Bruijn representation.
-- | Parameters: Token List, Lambda level (as in term), 
-- Bound variables list - head is latest bound
parse :: [Token] -> Int -> [Char] -> Term
parse [(V c)] i bv = dbi c i bv

parse (x:xs) i bv = 

-- | De Bruijn indexing for variable
dbi :: Char -> Int -> [Char] -> Term
dbi c i bv = 
    if bi == (length bv)
        then (Var (char2idx c)+i) -- free
        else (Var bi)             -- bound
    where bi = getBoundIdx c

-- | Retrieves variable from bound list
-- length - unbound otherwise lambda level
getBoundIdx :: Char -> [Char] -> Int
getBoundIdx _ [] = 0
getBoundIdx c (x:xs) = 
    if c == x
        then 0
        else 1+(getBoundIdx xs)