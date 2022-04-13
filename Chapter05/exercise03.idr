
import Data.Vect
import System.File

readToBlank : IO (List String)
readToBlank = do x <- getLine
                 if (x == "")
                    then pure []
                    else do xs <- readToBlank
                            pure (x :: xs)


readAndSave : IO ()
readAndSave = do putStrLn "Enter data (terminate with blank linke):"
                 xs <- readToBlank
                 putStr "The Filename, please: "
                 filename <- getLine
                 f <- writeFile filename ((show xs) ++ "\n")
                 case f of
                      (Left error) => printLn (show error)
                      (Right ()) => putStrLn "done"


readVect : File -> IO (n ** Vect n String)
readVect file = do
    False <- (fEOF file) | True => pure (_ ** [])
    Right x <- fGetLine file | Left err => pure (_ ** [])
    (_ ** xs) <- readVect file
    pure (_ ** x :: xs)


readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
    Right file <- openFile filename Read
        | Left err => pure (_ ** [])
    readVect file
