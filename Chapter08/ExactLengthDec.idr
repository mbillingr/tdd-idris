import Data.Nat
import Data.Vect
import Decidable.Equality

exactLength : {m : Nat} -> (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength len input = case decEq m len of
                             Yes Refl => Just input
                             No contra => Nothing
