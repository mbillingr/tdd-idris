
same_cons : {xs : List a} -> {ys : List a} -> xs = ys -> x :: xs = x :: ys
same_cons Refl = cong (x ::) Refl


same_lists : {xs : List a} -> {ys : List a} -> x = y -> xs = ys -> x :: xs = y :: ys
same_lists Refl Refl = cong (x ::) Refl


data ThreeEq : {ty: Type} -> (a : ty) -> (b : ty)-> (c : ty) -> Type where
     Same : (x : ty) -> ThreeEq x x x


allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS x x x (Same x) = Same (S x)
