import data.Nat
import Data.Vect

append_nil : Vect m a -> Vect (plus m 0) a
append_nil xs = rewrite plusZeroRightNeutral m in xs

append_xs : Vect (S (plus m len)) a -> Vect (plus m (S len)) a
append_xs xs = rewrite sym (plusSuccRightSucc m len) in xs

append : Vect n a -> Vect m a -> Vect (m + n) a
append [] ys = append_nil ys
append (x :: xs) ys = append_xs (x :: append xs ys)
