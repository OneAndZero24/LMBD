module SRC.Processor.Parser(parse) where

import SRC.Structure.Term
import SRC.Structure.Token

-- | Wrapper around parseTerm
parse :: [Token] -> (Term, Bool) 
parse = (parseTerm 0 [] Nothing)

-- | Parses token list into Term tree,
-- also changes variables to De Bruijn representation.
-- | Parameters: Token List, Lambda level (as in term), 
-- Bound variables list - head is latest bound, Left operand
parseTerm :: Int -> [Char] -> Maybe Term -> [Token] -> (Term, Bool)
parseTerm _ _ x [] =
    case x of
        Nothing -> ((Var 0), False)
        Just t -> (t, True)
parseTerm i bv x ((V c):xs) =          -- Variable
    (uncurry (add2Terms x)) (parseTerm i bv (Just (dbi i bv c)) xs)
parseTerm i bv x ((L v):xs) =          -- λ abstraction
    if e
        then (uncurry (add2Terms x)) ((Abs it), e)
        else ((Var 0), False)
    where (it, e) = parseTerm (i+1) (v:bv) Nothing xs
parseTerm i bv x (LB:xs) =             -- Brackets
    case selectBracket xs of
        Nothing -> ((Var 0), False) -- Bracket missmatch
        Just (a, b) -> 
            if e
                then (uncurry (add2Terms x)) (parseTerm i bv (Just it) b)
                else ((Var 0), False)
            where (it, e) = parseTerm i bv Nothing a
parseTerm _ _ _ _ = ((Var 0), False)   -- Error

add2Terms :: Maybe Term -> Term -> Bool -> (Term, Bool)
add2Terms x t e =
    case x of
        Nothing -> (t, e)
        Just tx -> ((App tx t), e)

selectBracket :: [Token] -> Maybe ([Token], [Token])
selectBracket = (curry (bracket LB RB 1)) []

-- | De Bruijn indexing for variable
dbi :: Int -> [Char] -> Char -> Term
dbi i bv c = 
    if bi == (length bv)
        then (Var ((char2Idx c)+i)) -- free
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
char2Idx :: Char -> Int
char2Idx c 
    | n < 97 = 0
    | n >= 97 && n <= 122 = n-97
    | n > 122 = 25
    where n = fromEnum c