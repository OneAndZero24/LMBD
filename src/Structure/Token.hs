module SRC.Structure.Token(Token(..), isaz, bracket) where

-- | Intermediate represenation of input. 
-- | Generated by Lexer, processed into Terms by Parser.
data Token =
    V Char  |
    L Char  |
    LB      |
    RB
    deriving (Eq, Show) -- for debug

-- | Application is left associative.
-- | Abstraction is right associative.

-- | Checks whether variable is in legal range
isaz :: Char -> Bool
isaz c = (n >= 97) && (n <= 122)
    where n = fromEnum c

-- | Brackets
bracket :: Eq a => a -> a -> Int -> ([a], [a]) -> Maybe ([a], [a])
bracket _ _ _ (_, []) = Nothing 
bracket lb rb i (l, (x:xs)) =
    if x == lb
        then bracket lb rb (i+1) (lb:l, xs)
        else
            if x == rb
                then
                    if i == 1
                        then Just ((reverse l), xs)
                        else bracket lb rb (i-1) (rb:l, xs)
                else bracket lb rb i (x:l, xs)