import Data.List

data SnocList : List a -> Type where
     Empty : SnocList []
     Snoc : (x, xs : _) -> (rec : SnocList xs) -> SnocList (xs ++ [x])

snocListHelp : {input : _} -> (snoc : SnocList input) -> (rest : List a) -> SnocList (input ++ rest)
snocListHelp snoc [] = rewrite appendNilRightNeutral input in snoc
snocListHelp snoc (x :: xs) = rewrite appendAssociative input [x] xs in snocListHelp (Snoc x input snoc) xs

snocList : (xs : List a) -> SnocList xs
snocList xs = snocListHelp Empty xs


myReverseHelper : (input : List a) -> SnocList input -> List a
myReverseHelper [] Empty = []
myReverseHelper (xs ++ [x]) (Snoc x xs rec) = x :: myReverseHelper xs rec

myReverse : List a -> List a
myReverse xs = myReverseHelper xs (snocList xs)


myReverseWith : List a -> List a
myReverseWith input with (snocList input)
  myReverseWith [] | Empty = []
  myReverseWith (xs ++ [x]) | (Snoc x xs rec) = x :: myReverseWith xs | rec  -- note the call with |rec, which avoids rebuilding the SnocList


{- Does not work with Idris2
isSuffix : Eq a => List a -> List a -> Bool
isSuffix input1 input2 with (snocList input1)
  isSuffix [] input2 | Empty = True
  isSuffix (xs ++ [x]) input2 | (Snoc x xs xsrec) with (snocList input2)
    isSuffix (xs ++ [x]) [] | (Snoc x xs xsrec) | Empty = False
    isSuffix (xs ++ [x]) (ys ++ [y]) | (Snoc x xs xsrec) | (Snoc y ys ysrec) = if x == y then isSuffix xs ys | xsrec | ysrec
                                                                                         else False
-}

isSuffix : Eq a => List a -> List a -> Bool
isSuffix input1 input2 with (snocList input1, snocList input2)
  isSuffix _ _ | ((Snoc x xs xsrec), (Snoc y ys ysrec)) = (x == y) && (isSuffix _ _ | (xsrec, ysrec))
  isSuffix _ _ | (Empty, y) = True
  isSuffix _ _ | (x, Empty) = False
