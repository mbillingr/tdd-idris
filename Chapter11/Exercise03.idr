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


shell : ConsoleIO ()
shell = do PutStr ">> "
           cmd <- GetLine
           case parseCmd (words cmd) of
                Nothing => do PutStr "Invalid Command\n"; shell
                Just Exit => Quit ()
                Just (Cat x) => ?rhs_cat
                Just (Copy x y) => ?rhs_cpy

main : IO ()
main = do Just () <- run forever shell
             | Nothing => putStrLn "Ran out of fuel"
          putStrLn "Bye"
