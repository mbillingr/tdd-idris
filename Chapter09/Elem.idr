module Chapter09.Elem

import Data.Vect
import Decidable.Equality

public export
data Elem : a -> Vect k a -> Type where
     Here : Elem x (x :: xs)
     There : (later : Elem x xs) -> Elem x (y :: xs)

export
Uninhabited (Elem x []) where
    uninhabited Here impossible
    uninhabited (There _) impossible


oneInVector : Elem 1 [1,2,3]
oneInVector = Here

maryInVector : Elem "Mary" ["Peter", "Paul", "Mary"]
maryInVector = There (There Here)

fourNotInVector : Elem 4 [1,2,3] -> Void
fourNotInVector (There (There (There Here))) impossible
fourNotInVector (There (There (There (There later)))) impossible
