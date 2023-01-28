module SRC.Processor.Lexer(checkc, tokenize) where

import SRC.Structure.Token

-- | Checks for unallowed characters
checkc :: String -> Bool
checkc [] = True
checkc (x:xs)
    | x == '\\' = (checklmbd xs) && (checkc xs)
    | x == '.' = checkc xs
    | x == ' ' = checkc xs
    | x == '(' = checkc xs
    | x == ')' = checkc xs
    | otherwise = (isaz x)

-- | Check if lambda only binds proper variables.
checklmbd :: String -> Bool
checklmbd [] = False
checklmbd (x:xs)
    | x == '.' = True
    | x == ' ' = checklmbd xs
    | otherwise = (isaz x)&&(checklmbd xs)

-- | Tokenizes input,
-- Spaces mark end of application sequence, other spaces are ignored.
tokenize :: String -> [Token]
tokenize [] = []
tokenize (x:xs)
    | x == ' ' = tokenize xs
    | x == '(' = [LB]++(tokenize (snd (bracketsplit (xs, []))))++[RB]++(tokenize (fst (bracketsplit (xs, []))))
    | x /= '\\' = (V x):(tokenize xs) -- variables
    | otherwise = 
        if (length d) <= 1
            then [Lambda (filter isaz b)]++(tokenize d)++(tokenize c)
            else [Lambda (filter isaz b)]++[LB]++(tokenize d)++[RB]++(tokenize c)
        where 
            (a, b) = (lmbdsplit (xs, []))
            (c, d) = lmbdbody (ltrim a, [])

-- | For brackets
bracketsplit :: (String, String) -> (String, String)
bracketsplit ([], l) = ([], l)
bracketsplit ((')':as), l) = (as, l)
bracketsplit ((a:as), l) = bracketsplit (as, l++[a])  

-- | Splits lambda body in binding and rest after .
lmbdsplit :: (String, String) -> (String, String)
lmbdsplit ([], l) = ([], l)
lmbdsplit (('.':as), l) = (as, l)
lmbdsplit ((a:as), l) = lmbdsplit (as, l++[a])  

-- | Extracts lambda body
lmbdbody :: (String, String) -> (String, String)
lmbdbody ([], l) = ([], l)
lmbdbody ((' ':as), l) = (as, l)
lmbdbody ((a:as), l) = lmbdbody (as, l++[a])  

-- | TODO: These kinds of functions appear several times throughout the code,
-- need to abstract them. Will do it later due to deadlines.

-- | Removes leading spaces
ltrim :: String -> String
ltrim (' ':xs) = ltrim xs
ltrim l = l
