import Data.Vect

allLengths : Vect len String -> Vect len Nat
allLengths [] = []
allLengths (word :: words) = ?vh :: allLengths words
