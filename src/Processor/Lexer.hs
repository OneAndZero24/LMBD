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
    | otherwise = (isaz x) && (checkc xs)

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
    | x == '(' = [LB]++(tokenize (snd (bracketsplit 1 (xs, []))))++[RB]++(tokenize (fst (bracketsplit 1 (xs, []))))
    | x == '\\' = (fst (lmbd (x:xs)))++(tokenize (snd (lmbd (x:xs))))
    | otherwise = (V x):(tokenize xs) -- variables

-- | Process lambda
lmbd :: String -> ([Token], String)
lmbd ('\\':xs)
    | (head d) == '(' = (l++[LB]++(tokenize (snd (bracketsplit 1 ((tail a), []))))++[RB], fst (bracketsplit 1 ((tail a), [])))
    | (head d) == '\\' = (l++[LB]++(fst (lmbd a))++[RB], (snd (lmbd a)))
    | otherwise = (l++[LB]++(tokenize d)++[RB], c)
    where
        (a, b) = (lmbdsplit (xs, []))
        (c, d) = lmbdbody (ltrim a, [])
        l = [Lambda (filter isaz b)]

-- | For brackets
bracketsplit :: Int -> (String, String) -> (String, String)
bracketsplit _ ([], l) = ([], l)
bracketsplit 1 ((')':as), l) = (as, l)
bracketsplit i ((')':as), l) = bracketsplit (i-1) (as, l++[')'])
bracketsplit i (('(':as), l) = bracketsplit (i+1) (as, l++['('])
bracketsplit i ((a:as), l) = bracketsplit i (as, l++[a])  

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
