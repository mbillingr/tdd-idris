
import Data.Vect
import Decidable.Equality


headUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} ->
    (contra : (x = y) -> Void) ->
    ((x :: xs) = (y :: ys)) -> Void
headUnequal contra Refl = contra Refl

tailUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} ->
    (contra : (xs = ys) -> Void) ->
    ((x :: xs) = (y :: ys)) -> Void
tailUnequal contra Refl = contra Refl


headTailEqual : DecEq a => 
    (x : a) -> 
    (y : a) -> 
    (xs : (Vect n a)) -> 
    (ys : (Vect n a)) -> 
    (prf_h : x = y) -> 
    (prf_t : xs = ys) -> 
    (x :: xs) = (y :: ys)
headTailEqual x x [] [] Refl Refl = Refl
headTailEqual x x xs xs Refl Refl = Refl

decEqImpl : DecEq a => (xs : Vect n a) -> (ys : Vect n a) -> Dec (xs = ys)
decEqImpl [] [] = Yes Refl
decEqImpl (x :: xs) (y :: ys) = case decEq x y of
                                     Yes prf_h => case decEqImpl xs ys of
                                                       Yes prf_t => Yes (headTailEqual x y xs ys prf_h prf_t)
                                                       No contra => No (tailUnequal contra)
                                     No contra => No (headUnequal contra)

DecEq a => DecEq (Vect n a) where
    decEq = decEqImpl