module SRC.Structure.Token(Token(..), isaz, bracket) where

-- | Intermediate represenation of input. 
-- | Represents input expression after being processed by LEXER
-- which adds additional/missing brackets that are often ommited.
-- | Application is left associative.
-- | Abstraction is right associative.
-- | List of Tokens is input for parser 
-- which transforms it into tree of λ terms
data Token =
    V Char  |       -- Variable
    L Char  |       -- Lambda Abstraction (λc. ...)
    LB      |       -- (
    RB              -- )
    deriving (Eq, Show) -- Show for debug

-- | Checks whether variable is in legal range [a-z].
isaz :: Char -> Bool
isaz c = (n >= 97) && (n <= 122)
    where n = fromEnum c

-- | Abstracted away helper function that splits expression
-- into "(a) b". Used to process brackets. Called when ( is detected
-- with level 1 and without this first opening bracket.
-- | First two arguments are special elements marking ( & ),
-- they are followed by "bracket level" and current (a, b) pair.
-- | Returns Nothing on error.
bracket :: Eq a => a -> a -> Int -> ([a], [a]) -> Maybe ([a], [a])
bracket _ _ _ (_, []) = Nothing
bracket lb rb i (l, (x:xs)) =
    if x == lb
        then bracket lb rb (i+1) (lb:l, xs) -- ( increase "bracket level"
        else if x == rb
            then if i == 1
                then Just ((reverse l), xs) -- back to level 1 - return
                else bracket lb rb (i-1) (rb:l, xs) -- ( decrease "bracket level"
            else bracket lb rb i (x:l, xs) -- other element