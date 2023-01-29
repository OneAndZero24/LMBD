-- Jan Miksa
import System.Environment

import SRC.Structure.Term
import SRC.Processor.Lexer
import SRC.Processor.Parser
import SRC.Processor.Evaluator

-- | Arguments
-- i - repl - if not set expects file to read
-- g - greedy eval mode
-- s - single Beta reduction step mode 
main :: IO ()
main = do
    args <- getArgs
    let e = if elem "-g" args 
        then eval
        else if elem "-s" args
            then reduce
            else reduce2Norm
    if elem "-i" args
        then (repl e)
        else (fileE e (head (filterFileName args)))

-- | Filter filename from arguments
filterFileName :: [String] -> [String]
filterFileName s = filter (\x -> not (elem x ["-i", "-s", "-g"])) s

-- | REPL loop mode
-- Takes in evaluator as argument
repl :: (Term -> Term) -> IO ()
repl f = do
    putStr "\\>"
    l <- getLine
    case (head l) of
        '>' -> do repl f   -- comment
        '!' -> return ()   -- exit
        otherwise -> do
            case (tokenize l) of
                Nothing -> do
                    putStrLn "Syntax error!"
                    repl f
                (Just t) -> do
                    if not e
                        then do
                            putStrLn "Syntax error!"
                            repl f
                        else do 
                            print (f x)
                            repl f
                    where (x, e) = parse t

-- | File input mode
fileE :: (Term -> Term) -> String -> IO ()
fileE f s = do 
    c <- readFile s
    let l = lines c
    let lc = filter (\(x:xs) -> (x /= '>')) l  -- Filters out comment lines
    case (tokenize (concat lc)) of
        Nothing -> do
            putStrLn "Syntax error!"
        (Just t) -> do
            if not e
                then do
                    putStrLn "Syntax error!"
                else do 
                    print (f x)
            where (x, e) = parse t