import SRC.Processor.Lexer
import SRC.Processor.Parser
import SRC.Processor.Evaluator

repl :: IO ()
repl = do
    putStr "\\>"
    l <- getLine
    case (head l) of
        '>' -> do repl   -- Comment
        '!' -> return () -- Exit
        otherwise -> do
            if not (checkc l)
                then do
                    putStrLn "Syntax error!"
                    repl
                else do
                    let t = (tokenize l)
                    if not (check t) 
                        then do
                            putStrLn "Syntax error!"
                            repl
                        else do
                            let s = (process t)
                            print (reduce2Norm s)
                            repl