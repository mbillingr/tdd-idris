import Tree
import Chapter04


-- exercise 1

listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)


-- exercise 2

treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node left x right) = treeToList left ++ [x] ++ treeToList right


-- exercise 3

data Expr = Val Integer
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr


-- exercise 4

evaluate : Expr -> Integer
evaluate (Val x) = x
evaluate (Add x y) = evaluate x + evaluate y
evaluate (Sub x y) = evaluate x - evaluate y
evaluate (Mul x y) = evaluate x * evaluate y


-- exercise 5

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing y = y
maxMaybe x Nothing = x
maxMaybe (Just x) (Just y) = Just (max x y)


-- exercise 6

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive tri@(Triangle _ _)) = Just (area tri)
biggestTriangle (Primitive _) = Nothing
biggestTriangle (Combine pic pic1) = maxMaybe (biggestTriangle pic) (biggestTriangle pic1)
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic
