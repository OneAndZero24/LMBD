module Processor.Lexer(checkc, tokenize) where

import Structure.Token

checkc :: String -> Bool
checkc [] = True
checkc (x:xs)
    | x == '\\' = checkc xs
    | x == '.' = checkc xs
    | x == ' ' = checkc xs
    | x == '(' = checkc xs
    | x == ')' = checkc xs
    | otherwise = (isaz x)

