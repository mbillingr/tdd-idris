module Chapter09.ElemType

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


notInNil : Elem value [] -> Void
notInNil Here impossible
notInNil (There _) impossible

notInTail : (notHere : (value = x) -> Void) -> (notThere : Elem value xs -> Void) -> Elem value (x :: xs) -> Void
notInTail notHere notThere Here = notHere Refl
notInTail notHere notThere (There later) = notThere later

isElem : DecEq ty => (value : ty) -> (xs : Vect n ty) -> Dec (Elem value xs)
isElem value [] = No (notInNil)
isElem value (x :: xs) = case decEq value x of
                              Yes Refl => Yes Here
                              No notHere => case isElem value xs of
                                                 Yes prf => Yes (There prf)
                                                 No notThere => No (notInTail notHere notThere)
