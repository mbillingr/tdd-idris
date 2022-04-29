
data InfIO : Type where
     Do : IO a -> (a -> Inf InfIO) -> InfIO
     Seq : IO () -> Inf InfIO -> InfIO


(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do

(>>) : IO () -> Inf InfIO -> InfIO
(>>) = Seq

loopPrint : String -> InfIO
loopPrint msg = do putStrLn msg
                   loopPrint msg


data Fuel = Dry | More (Lazy Fuel)

forever : Fuel
forever = More forever

run : Fuel -> InfIO -> IO ()
run (More fuel) (Do action cont) = do res <- action
                                      run fuel (cont res)
run (More fuel) (Seq action cont) = do action
                                       run fuel cont
run Dry p = putStrLn "Out of fuel"
