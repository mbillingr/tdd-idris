import Data.Fuel
import Data.String
import System.File.ReadWrite


data Command : Type -> Type where
     PutStr : String -> Command ()
     GetLine : Command String
     ReadFile : String -> Command (Either FileError String)
     WriteFile : String -> String -> Command (Either FileError ())


data ConsoleIO : Type -> Type where
     Quit : a -> ConsoleIO a
     Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
    

(>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
(>>=) = Do

%tcinline
(>>) : Command () -> Inf (ConsoleIO b) -> ConsoleIO b
ma >> mb = Do ma (\ _ => mb)


runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
runCommand (ReadFile fn) = readFile fn
runCommand (WriteFile fn x) = writeFile fn x


run : Fuel -> ConsoleIO a -> IO (Maybe a)
run Dry _ = pure Nothing
run _ (Quit val) = pure (Just val)
run (More fuel) (Do c f) = do res <- runCommand c
                              run fuel (f res)


data ShellCmd = Exit | Cat String | Copy String String

parseCmd : List String -> Maybe ShellCmd
parseCmd ["exit"] = Just Exit
parseCmd ["cat", file] = Just (Cat file)
parseCmd ["copy", src, dst] = Just (Copy src dst)
parseCmd _ = Nothing

mutual
     shell : ConsoleIO ()
     shell = do PutStr ">> "
                cmd <- GetLine
                case parseCmd (words cmd) of
                     Nothing => do PutStr "Invalid Command\n"; shell
                     Just Exit => Quit ()
                     Just (Cat filename) => catFile filename
                     Just (Copy src dst) => copyFile src dst

     catFile : String -> ConsoleIO ()
     catFile filename = do result <- ReadFile filename
                           case result of
                                (Left err) => PutStr (show err ++ "\n")
                                (Right content) => PutStr content
                           shell

     copyFile : String -> String -> ConsoleIO ()
     copyFile src dst = do result <- ReadFile src
                           case result of
                                (Left err) => do PutStr (show err ++ "\n")
                                                 shell
                                (Right content) => toFile dst content

     toFile : String -> String -> ConsoleIO ()
     toFile dst content = do result2 <- WriteFile dst content
                             case result2 of
                                  (Left err) => PutStr (show err ++ "\n")
                                  (Right ()) => PutStr "OK"
                             shell

main : IO ()
main = do Just () <- run forever shell
             | Nothing => putStrLn "Ran out of fuel"
          putStrLn "Bye"
