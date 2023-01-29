module SRC.Processor.Lexer(tokenize) where

import SRC.Structure.Token

-- | Wrapper around token parsing.
tokenize :: String -> Maybe [Token]
tokenize = (token []).filterS

-- | Filters spaces.
filterS :: String -> String
filterS = (filter ((/=) ' '))

-- | Transforms input string to token list.
-- | Fills out missing/implicit brackets for PARSER.
-- | Produces redundant brackets, loosely based on
-- operator precedence parsing algorithm tailored for
-- λ calculus operators.
-- | Parameters: LHS operand buffer (reversed, last in input string is first),
-- input string. Uses Maybe for error raporting.
token :: [Token] -> String -> Maybe [Token]
token [] "" = Just []            -- recursion end
token ts "" = Just (reverse ts)  -- --||--
token ts ('(':xs) =              -- ( -> call on internal part off input, use brackets to apply it properly
    (selectBracket xs) >>= (\(a, b) -> 
        let at = (concatB (token ts "") (wrap (token [] a)) ) in
        case at of
            Nothing -> Nothing
            Just t -> token (reverse t) b
    )
token [] ('\\':xs) = lmbd xs     -- λ abstraction, note: no need to parse '.' as it's handled by lmbd
token (t:ts) ('\\':xs) = 
    case t of
        RB -> concatB (token (t:ts) "") (lmbd xs)
        (V _) -> concatB (token (t:ts) "") (lmbd xs)
        otherwise -> lmbd xs                -- λ application only if LHS ends with other var or )
token [] (x:xs) =                -- other characters - must be variables
    if (isaz x)
        then token [(V x)] xs
        else Nothing        -- Illegal character
token (t:ts) (x:xs) = 
    if (isaz x)
        then case t of
            RB -> token ([RB]++[(V x)]++(t:ts)++[LB]) xs
            (V _) -> token ([RB]++[(V x)]++(t:ts)++[LB]) xs
            otherwise -> token ((V x):ts) xs -- λ application only if LHS ends with other var or )
        else Nothing        -- Illegal character

-- | NOTE: Colud have abstracted some parts.

-- | Processes λ abstraction.
lmbd :: String -> Maybe [Token]
lmbd "" = Nothing      -- error, empty lambda
lmbd ('.':xs) = wrap (token [] xs)  -- end binding part, wrap body in ( )
lmbd ('\\':xs) = lmbd xs    -- λ abstraction
lmbd (x:xs) = 
    if (isaz x)
        then concatB (Just [(L x)]) (lmbd xs) -- bind & process rest
        else Nothing

-- | Wraps in ( ) handling errors and [].
wrap :: Maybe [Token] -> Maybe [Token] 
wrap Nothing = Nothing
wrap (Just []) = Just []
wrap (Just ts) = Just ([LB]++ts++[RB])

-- | Handles λ application brackets, handling errors and [].
concatB :: Maybe [Token] -> Maybe [Token] -> Maybe [Token]
concatB Nothing _ = Nothing
concatB _ Nothing = Nothing
concatB (Just t1) (Just []) = Just t1
concatB (Just []) (Just t2) = Just t2
concatB (Just t1) (Just t2) = wrap (Just (t1++t2))

-- Wrapper around Token.bracket for Char.
selectBracket :: String -> Maybe (String, String)
selectBracket = (curry (bracket '(' ')' 1)) []