
data RunIO : Type -> Type where
     Quit : a -> RunIO a
     Do : IO a -> (a -> Inf (RunIO b)) -> RunIO b
     Seq : IO () -> Inf (RunIO b) -> RunIO b


(>>=) : IO a -> (a -> Inf (RunIO b)) -> RunIO b
(>>=) = Do

(>>) : IO () -> Inf (RunIO b) -> RunIO b
(>>) = Seq


data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever


run : Fuel -> RunIO a -> IO (Maybe a)
run Dry _ = pure Nothing
run fuel (Quit value) = pure (Just value)
run (More fuel) (Do c f) = do res <- c; run fuel (f res)
run (More fuel) (Seq x k) = do x; run fuel k


greet : RunIO ()
greet = do putStr "Enter your name: "
           name <- getLine
           if name == ""
              then do putStrLn "Bye bye!"
                      Quit ()
              else do putStrLn ("Hello " ++ name)
                      greet

main : IO ()
main = do _ <- run forever greet
          pure ()