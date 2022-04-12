
printLength : IO ()
printLength = getLine >>= \input => let len = length input in printLn len

printLength_do : IO ()
printLength_do = do putStr "Input String: "
                    input <- getLine
                    let len = length input
                    printLn len
