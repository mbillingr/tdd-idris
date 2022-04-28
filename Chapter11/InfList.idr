
data InfList : Type -> Type where
     (::) : (value : x) -> Inf (InfList x) -> InfList x

%name InfList xs, ys, zs

countFrom : Integer -> InfList Integer
countFrom x = x :: Delay (countFrom (x + 1))

getPrefix : (count : Nat) -> InfList ty -> List ty
getPrefix 0 xs = []
getPrefix (S k) (value :: xs) = value :: getPrefix k (Force xs)
