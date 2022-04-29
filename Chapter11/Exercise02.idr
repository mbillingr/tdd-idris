
import Chapter11.ArithTotal

totalREPL : (prompt : String) -> (action : String -> String) -> InfIO
totalREPL prompt action = do putStr prompt
                             input <- getLine
                             putStr (action input)
                             totalREPL prompt action
