-- Jan Miksa
import System.Environment
import System.IO

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
    if (length args) /= 0
        then do
            let e = if elem "-g" args 
                then (eval)
                else if elem "-s" args
                    then (reduce)
                    else (reduce2Norm)
            if elem "-i" args
                then do 
                    repl e
                    return ()
                else do (fileE e (head (filterFileName args)))
        else do
            putStrLn("      ___       ___           ___           ___")       
            putStrLn("     /\\__\\     /\\__\\         /\\  \\         /\\  \\")  
            putStrLn("    /:/  /    /::|  |       /::\\  \\       /::\\  \\")  
            putStrLn("   /:/  /    /:|:|  |      /:/\\:\\  \\     /:/\\:\\  \\") 
            putStrLn("  /:/  /    /:/|:|__|__   /::\\~\\:\\__\\   /:/  \\:\\__\\") 
            putStrLn(" /:/__/    /:/ |::::\\__\\ /:/\\:\\ \\:|__| /:/__/ \\:|__|")
            putStrLn(" \\:\\  \\    \\/__/~~/:/  / \\:\\~\\:\\/:/  / \\:\\  \\ /:/  /")
            putStrLn("  \\:\\  \\         /:/  /   \\:\\ \\::\\  \\   \\:\\  /:/  /")
            putStrLn("   \\:\\  \\       /:/  /     \\:\\/:/  /     \\:\\/:/  /")
            putStrLn("    \\:\\__\\     /:/  /       \\::/  /       \\::/  /")
            putStrLn("     \\/__/     \\/__/         \\/__/         \\/__/")  
            putStrLn("---------------------------------- v.0.0.1 Jan Miksa-")
            putStrLn("| LMBD - Lambda expression evaluator/reducer.       |")
            putStrLn("| Usage:                                            |")
            putStrLn("| - lmbd \"test.lmbd\" - run file                     |") 
            putStrLn("| - [-i] - REPL                                     |")
            putStrLn("| - [-g] - greedy evaluation mode                   |")
            putStrLn("| - [-s] - perform only one beta reduction step     |")
            putStrLn("| - ! - quit interactive mode                       |")
            putStrLn("-----------------------------------------------------")      

-- | Filter filename from arguments
filterFileName :: [String] -> [String]
filterFileName s = filter (\x -> not (elem x ["-i", "-s", "-g"])) s

-- | REPL prompt
prompt :: String -> IO String
prompt t = do
    putStr t
    hFlush stdout
    getLine

-- | REPL loop mode
-- Takes in evaluator as argument
repl :: (Term -> Term) -> IO ()
repl f = do
    l <- (prompt "\\>")
    if l == [] then do repl f
        else case (head l) of
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