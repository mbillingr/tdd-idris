
import Data.Vect
import Chapter09.Elem

removeElem : {n : Nat} -> 
             (value : a) -> 
             (xs : Vect (S n) a) -> 
             (prf : Elem value xs) ->
             Vect n a
removeElem value (value :: ys) Here = ys
removeElem {n = Z} value (y :: []) (There later) = absurd later
removeElem {n = (S k)} value (y :: ys) (There later) = y :: (removeElem value ys later)
