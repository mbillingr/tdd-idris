
import System

readNumber : IO (Maybe Nat)
readNumber = do
    input <- getLine
    if all isDigit (unpack input)
       then pure (Just (cast input))
       else pure Nothing

guess : (target : Nat) -> Nat -> IO ()
guess target nth = do
    putStr "Your guess #"
    print (S nth)
    putStr ": "
    Just input <- readNumber
        | Nothing => do putStrLn "Invalid Input"
                        guess target nth
    case compare input target of
        LT => do putStrLn "too low!"
                 guess target (S nth)
        EQ => do putStrLn "Correct!"
                 pure ()
        GT => do putStrLn "too high!"
                 guess target (S nth)


rand : IO Nat
rand = do t <- time
          pure (cast ((mod t 100) + 1))


main : IO ()
main = do
    number <- rand
    guess number 0


repl : String -> (String -> String) -> IO ()
repl prompt ep = do
    putStr prompt
    input <- getLine
    let result = ep input
    putStrLn result
    repl prompt ep
