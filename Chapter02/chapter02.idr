import Data.String
import Data.List

my_the : (ty: Type) -> ty -> ty
my_the _ x = x


double : Num ty => ty -> ty
double x = x + x

twice : (a -> a) -> a -> a
twice f x = f (f x)

quadruple : Num a => a -> a
quadruple  = twice double


-- 2.2.7

longer : String -> String -> Nat
longer word1 word2
    = let len1 = length word1
          len2 = length word2 in
          if len1 > len2 then len1 else len2

pythagoras : Double -> Double -> Double
pythagoras x y = sqrt (square x + square y)
    where
        square : Double -> Double
        square x = x * x
