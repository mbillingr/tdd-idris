
import Chapter08.EqNat

data Vect : Nat -> Type -> Type where
     Nil : Vect Z a
     (::) : a -> Vect k a -> Vect (S k) a


exactLength : {m: Nat} -> (len: Nat) -> (input: Vect m a) -> Maybe (Vect len a)
exactLength len input = case checkEqNat m len of
                             Nothing => Nothing
                             (Just (Same len)) => Just input
