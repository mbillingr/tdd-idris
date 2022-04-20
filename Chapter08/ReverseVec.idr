
import Data.Vect

myReverse : Vect n a -> Vect n a
myReverse [] = []
myReverse (x :: xs) =  reverseProof (myReverse xs ++ [x]) 
    where
        reverseProof : Vect (plus len 1) a -> Vect (S len) a
        reverseProof result = rewrite plusCommutative 1 len in result

