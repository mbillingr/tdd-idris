
import Data.List
import Decidable.Equality


data Elem : a -> List a -> Type where
     Here : Elem x (x :: xs)
     There : (later : Elem x xs) -> Elem x (y :: xs)


data Last : List a -> a -> Type where
     LastOne : Last [value] value
     LastCons : (prf : Last xs value) -> Last (x :: xs) value

Uninhabited (Last [] x) where
    uninhabited LastOne impossible
    uninhabited (LastCons prf) impossible

noLastNil : Last [] value -> Void
noLastNil LastOne impossible
noLastNil (LastCons prf) impossible

lastNotEq : (x = value -> Void) -> Last [x] value -> Void
lastNotEq contra LastOne = contra Refl
lastNotEq _ (LastCons LastOne) impossible
lastNotEq _ (LastCons (LastCons prf)) impossible

notLastCons : (Last xs value -> Void) -> Last (_ :: xs) value -> Void
notLastCons contra LastOne = ?rhs  -- no idea how to fill this hole, but I think this is an impossible case
notLastCons contra (LastCons prf) = contra prf

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No (noLastNil)
isLast [x] value = case decEq x value of
                        Yes Refl => Yes LastOne
                        No contra => No (lastNotEq contra)
isLast (_ :: xs) value = case isLast xs value of
                              Yes prf => Yes (LastCons prf)
                              No contra => No (notLastCons contra)
