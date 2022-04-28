
-- note: run idris2 with `--package contrib` to get access to the contrib library

import Data.List.Views
import Data.List.Views.Extra
import Data.Nat.Views
import Data.Vect
import Data.Vect.Views.Extra

-- Exercise 1

equalSuffix : Eq a => List a -> List a -> List a
equalSuffix input1 input2 with (snocList input1, snocList input2)
  equalSuffix (xs ++ [x]) (ys ++ [y]) | ((Snoc x xs xsrec), (Snoc y ys ysrec))
    = let suffix = (equalSuffix _ _ | (xsrec, ysrec)) in
          if x == y then suffix ++ [x]
                    else []
  equalSuffix _ _ | (_, _) = []


-- Exercise 2

{-  (could not get it to work...)
mergeSort : Ord a => {n: Nat} -> Vect n a -> Vect n a
mergeSort input with (splitRec input)
  mergeSort [] | SplitRecNil = []
  mergeSort [x] | SplitRecOne = [x]
  mergeSort (xs ++ ys) | (SplitRecPair xsrec ysrec) = merge (mergeSort xs | xrec) (mergeSort ys | yrec)
 -}

 -- Exercise 3

toBinary : Nat -> String
toBinary n with (halfRec n)
  toBinary Z | HalfRecZ = ""
  toBinary (k + k) | (HalfRecEven k rec) = toBinary k ++ "0"
  toBinary (S (k + k)) | (HalfRecOdd k rec) = toBinary k ++ "1"

-- Exercise 4

palindrome : Eq a => List a -> Bool
palindrome xs with (vList xs)
  palindrome [] | VNil = True
  palindrome [x] | VOne = True
  palindrome (x :: (xs ++ [y])) | (VCons rec) = (x == y) && (palindrome _ | rec)
