import Chapter07.Expr

Functor Expr where
     map func (Val x) = Val (func x)
     map func (Add x y) = Add (map func x) (map func y)
     map func (Sub x y) = Sub (map func x) (map func y)
     map func (Mul x y) = Mul (map func x) (map func y)
     map func (Div x y) = Div (map func x) (map func y)
     map func (Abs x) = Abs (map func x)


data MyVect : Nat -> Type -> Type where
     Nil : MyVect Z a
     (::) : (x : a) -> (xs : MyVect k a) -> MyVect (S k) a

(Eq a) => Eq (MyVect n a) where
     ([] == []) = True
     ((x :: xs) == (y :: ys)) = x == y && xs == ys

Foldable (MyVect n) where
     foldr func acc [] = acc
     foldr func acc (x :: xs) = func x (foldr func acc xs)
