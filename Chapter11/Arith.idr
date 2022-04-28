import Data.Primitives.Views

quiz : Stream Int -> (score : Nat) -> IO ()
quiz (num1 :: num2 :: nums) score
     = do putStrLn ("Score so far: " ++ show score)
          putStr (show num1 ++ " * " ++ show num2 ++ "? ")
          answer <- getLine
          if cast answer == num1 * num2
             then do putStrLn "Correct!"
                     quiz nums (score + 1)
             else do putStrLn ("Wrong, the answer is " ++ show (num1 * num2))
                     quiz nums score

shr : Int -> Nat -> Int
shr n Z = n
shr n (S k) = shr (n `div` 2) k

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shr` 2) :: randoms seed'

arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed)
    where
        bound : Int -> Int
        bound num with (divides num 12)
          bound ((12 * div) + rem) | (DivBy div rem prf) = rem + 1