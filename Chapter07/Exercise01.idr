
import Data.List

data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

%name Shape shape, shape1, shape2

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

Eq Shape where
  (==) (Triangle h1 b1) (Triangle h2 b2) = h1 == h2 && b1 == b2
  (==) (Rectangle h1 w1) (Rectangle h2 w2) = h1 == h2 && w1 == w2
  (==) (Circle r1) (Circle r2) = r1 == r2
  (==) _ _ = False

Ord Shape where
  compare shape1 shape2 = compare (area shape1) (area shape2)


testShapes : List Shape
testShapes = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4, Rectangle 2 7]