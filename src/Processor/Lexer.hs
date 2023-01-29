module SRC.Processor.Lexer(tokenize) where

import SRC.Structure.Token

-- | Transforms input string to tokens.
-- | Fills out missing brackets for parser.
tokenize :: String -> Maybe [Token]
tokenize = (token []).filterS

-- | Ignores spaces
filterS :: String -> String
filterS = (filter ((/=) ' '))

-- | Splits into processing each bracket separately
token :: [Token] -> String -> Maybe [Token]
token [] "" = Just []
token ts "" = Just (reverse ts)
token ts ('(':xs) = 
    (selectBracket xs) >>= (\(a, b) -> 
        let at = (concatB (token ts "") (wrap (token [] a)) ) in
        case at of
            Nothing -> Nothing
            Just t -> token (reverse t) b
    )
token [] ('\\':xs) = lmbd xs 
token (t:ts) ('\\':xs) = 
    case t of
        RB -> concatB (token (t:ts) "") (lmbd xs)
        (V _) -> concatB (token (t:ts) "") (lmbd xs)
        otherwise -> lmbd xs 
token [] (x:xs) =
    if (isaz x)
        then token [(V x)] xs
        else Nothing
token (t:ts) (x:xs) = 
    if (isaz x)
        then case t of
            RB -> token ([RB]++[(V x)]++(t:ts)++[LB]) xs
            (V _) -> token ([RB]++[(V x)]++(t:ts)++[LB]) xs
            otherwise -> token ((V x):ts) xs
        else Nothing

lmbd :: String -> Maybe [Token]
lmbd "" = Nothing
lmbd ('.':xs) = wrap (token [] xs)
lmbd ('\\':xs) = lmbd xs
lmbd (x:xs) = 
    if (isaz x)
        then concatB (Just [(L x)]) (lmbd xs)
        else Nothing

wrap :: Maybe [Token] -> Maybe [Token] 
wrap Nothing = Nothing
wrap (Just []) = Just []
wrap (Just ts) = Just ([LB]++ts++[RB])

concatB :: Maybe [Token] -> Maybe [Token] -> Maybe [Token]
concatB Nothing _ = Nothing
concatB _ Nothing = Nothing
concatB (Just t1) (Just []) = Just t1
concatB (Just []) (Just t2) = Just t2
concatB (Just t1) (Just t2) = wrap (Just (t1++t2))

selectBracket :: String -> Maybe (String, String)
selectBracket = (curry (bracket '(' ')' 1)) []