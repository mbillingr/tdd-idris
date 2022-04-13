
getStr : String -> IO String
getStr prompt = do putStr prompt
                   getLine

printLonger : IO ()
printLonger = do str1 <- getStr "Input String 1: "
                 str2 <- getStr "Input String 2: "
                 let len1 = length str1
                     len2 = length str2
                 printLn (max len1 len2)

printLonger2 : IO ()
printLonger2 = getStr "Input String 1: " >>= \str1 => 
               getStr "Input String 2: " >>= \str2 =>
               let len1 = length str1
                   len2 = length str2 in
                   printLn (max len1 len2)
