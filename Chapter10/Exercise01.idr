
import Data.Nat

data TakeN : List a -> Type where
     Fewer : TakeN xs
     Exact : (n_xs : List a) -> {rest : _} -> TakeN (n_xs ++ rest)

takeN : (n : Nat) -> (xs : List a) -> TakeN xs
{-takeN n xs = takeNHelper [] n xs
    where
        takeNHelper : (n_xs : List a) -> (n : Nat) -> (xs : List a) -> TakeN xs
        takeNHelper n_xs 0 xs = Exact n_xs
        takeNHelper _ _ [] = Fewer
        takeNHelper n_xs (S k) (x :: xs) = ?rhs
-}
takeN 0 xs = Exact []
takeN n [] = Fewer
takeN (S k) (x :: xs) = case takeN k xs of
                             Fewer => Fewer
                             Exact n_xs => Exact (x :: n_xs)

groupByN : (n : Nat) -> (xs : List a) -> List (List a)
groupByN n xs with (takeN n xs)
  groupByN n xs | Fewer = [xs]
  groupByN n (n_xs ++ rest) | (Exact n_xs) = n_xs :: (groupByN n rest)


halves : List a -> (List a, List a)
halves xs = let n = (length xs) `div` 2 in 
                case takeN n xs of
                     Fewer => ([], xs)
                     Exact n_xs {rest} => (n_xs, rest)
