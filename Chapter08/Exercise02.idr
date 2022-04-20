
import Data.Nat
import Data.Vect


myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes 0 m = rewrite plusZeroRightNeutral m in Refl
myPlusCommutes (S k) m = rewrite myPlusCommutes k m in plusSuccRightSucc m k


reverseProof_nil : Vect k a -> Vect (plus k 0) a
reverseProof_nil ys = rewrite plusZeroRightNeutral k in ys

reverseProof_xs : Vect (S (plus k len)) a -> Vect (plus k (S len)) a
reverseProof_xs ws = rewrite sym (plusSuccRightSucc k len) in ws

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
    where reverse' : Vect k a -> Vect l a -> Vect (k+l) a
          reverse' acc [] = reverseProof_nil acc
          reverse' acc (x :: xs) = reverseProof_xs (reverse' (x::acc) xs)
