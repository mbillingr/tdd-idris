import Data.Bits
import Data.Primitives.Views
import Data.String
import System


data Command : Type -> Type where
     PutStr : String -> Command ()
     GetLine : Command String


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

data Fuel = Dry | More (Lazy Fuel)

forever : Fuel
forever = More forever

run : Fuel -> ConsoleIO a -> IO (Maybe a)
run Dry _ = pure Nothing
run _ (Quit val) = pure (Just val)
run (More fuel) (Do c f) = do res <- runCommand c
                              run fuel (f res)


mutual
    correct : Stream Int -> (score : Nat) -> ConsoleIO Nat
    correct nums score = do PutStr "Correct!\n"
                            quiz nums (score + 1)

    wrong : Stream Int -> Int -> (score : Nat) -> ConsoleIO Nat
    wrong nums ans score = do PutStr ("Wrong, the answer is " ++ show ans ++ "\n")
                              quiz nums score

    quiz : Stream Int -> (score : Nat) -> ConsoleIO Nat
    quiz (num1 :: num2 :: nums) score
        = do PutStr ("Score so far: " ++ show score ++ "\n")
             PutStr (show num1 ++ " * " ++ show num2 ++ "? ")
             answer <- GetLine
             if toLower answer == "quit" then Quit score else
                if cast answer == num1 * num2
                    then correct nums score
                    else wrong nums (num1 * num2) score


randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'

arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed)
    where
        bound : Int -> Int
        bound num with (divides num 12)
          bound ((12 * div) + rem) | (DivBy div rem prf) = rem + 1


main : IO ()
main = do seed <- time
          Just score <- run forever (quiz (arithInputs (fromInteger seed)) 0)
             | Nothing => putStrLn "Ran out of fuel"
          putStrLn ("Final score: " ++ show score)
