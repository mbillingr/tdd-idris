import Data.Vect

fourInts : Vect 4 Int
fourInts = [1, 2, 3, 4]

sixInts : Vect 6 Int
sixInts = [4, 5, 6, 7, 8, 9]

tenInts : Vect 10 Int
tenInts = fourInts ++ sixInts
