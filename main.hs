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
                (Just t) -> do
                    if not e
                        then do
                            putStrLn "Syntax error!"
                            repl
                        else do 
                            print (reduce2Norm x)
                            repl
                    where (x, e) = parse t