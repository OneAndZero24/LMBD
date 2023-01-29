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
            case (tokenize l) of
                Nothing -> do
                    putStrLn "Syntax error!"
                    repl
                Just t -> do
                    case (process t) do
                        Nothing -> do
                            putStrLn "Syntax error!"
                            repl
                        Just x -> do 
                            print (reduce2Norm s)
                            repl