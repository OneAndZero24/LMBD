module SRC.Processor.Lexer(tokenize) where

import SRC.Structure.Token

-- | Transforms input string to tokens.
-- | Fills out missing brackets for parser.
tokenize :: String -> Maybe [Token]
tokenize = (split "").filterS

-- | Ignores spaces
filterS :: String -> String
filterS = filter.((\=) ' ')

-- | Splits into processing each bracket separately
split :: String -> String -> Maybe [Token]
split "" "" = Just []
split s [] = wrap (tokenizeChar (Just []) s)
split s ('(':xs) = 
    (selectBracket xs) >>= (\a b -> 
       wrap ((wrap (split "" s)++(wrap (split "" a)))++(split "" b))
    )
split a (x:xs) = split (a++[x]) xs

wrap :: Maybe [Token] -> Maybe [Token] 
wrap Nothing = Nothing
wrap (Just []) = Just []
wrap (Just ts) = Just ([LB]++ts++[RB])

tokenizeChar :: Maybe [Token] -> String -> Maybe [Token]
tokenizeChar s ('\\':xs) = wrap (lmbd xs)
tokenizeChar s (x:xs) =
    if (isaz x)
        then tokenizeChar (Just (wrap s)++[x]) xs
        else Nothing

lmbd :: String -> Maybe [Token]
lmbd ('.':xs) = wrap (tokenizeChar xs)
lmbd (x:xs) = 
    if (isaz x)
        then wrap (lmbd xs)
        else Nothing

selectBracket :: String -> Maybe (String, String)
selectBracket = (curry (bracket '(' ')' 1)) []