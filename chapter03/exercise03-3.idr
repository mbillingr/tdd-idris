import Data.Vect

-- exercise 1

createEmpties : {n : _ } -> Vect n (Vect 0 elem)
createEmpties = replicate _ []

transposeMat : {n : _ } -> Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                             zipWith (::) x xsTrans

-- exercise 2

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

-- exercise 3

zeros : Num a => {n : Nat} -> {p : _} -> Vect n (Vect p a)
zeros = replicate _ (replicate _ 0)

mulVecMat : Num a => {p : Nat} -> (x : Vect m a) -> (ys : Vect m (Vect p a)) -> Vect p a
mulVecMat [] [] = replicate _ 0
mulVecMat (x :: xs) (y :: ys) = zipWith (+) (map (* x) y) (mulVecMat xs ys)

mulMatrix : Num a => {n : Nat} -> {p : _} -> Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
mulMatrix [] [] = []
mulMatrix [] (x :: xs) = []
mulMatrix (x :: xs) [] = zeros
mulMatrix (x :: xs) ys = mulVecMat x ys :: mulMatrix xs ys
