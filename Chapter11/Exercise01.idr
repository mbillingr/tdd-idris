import Data.Primitives.Views

import Chapter11.Arith
import Chapter11.InfList

every_other : Stream a -> Stream a
every_other (_ :: x :: xs) = x :: every_other xs

Functor InfList where
    map f (x :: xs) = (f x) :: map f xs


data Face = Head | Tail

coinFlips : (count : Nat) -> Stream Int -> List Face
coinFlips 0 xs = []
coinFlips (S k) (x :: xs) = getFace x :: coinFlips k xs
    where
        getFace : Int -> Face
        getFace x with (divides x 2)
          getFace ((2 * div) + rem) | (DivBy div rem prf) = if rem == 0 then Head else Tail


square_root_approx : (number : Double) -> (approx : Double) -> Stream Double
square_root_approx number approx 
    = let next = (approx + (number / approx)) / 2 in
          approx :: square_root_approx number next


square_root_bound : (max : Nat) -> (number : Double) -> (bound : Double) -> (approxs : Stream Double) -> Double
square_root_bound 0 number bound (x :: _) = x
square_root_bound (S k) number bound (x :: xs)
    = let diff = abs (x * x - number) in
          if diff <= bound
             then x
             else square_root_bound k number bound xs

square_root : (number : Double) -> Double
square_root number = square_root_bound 100 number 0.0000000000001 (square_root_approx number number)