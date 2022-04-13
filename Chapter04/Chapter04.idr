
data Direction = North | East | South | West

turnClockwise : Direction -> Direction
turnClockwise North = East
turnClockwise East = South
turnClockwise South = West
turnClockwise West = North


||| Represents shapes
data Shape = ||| A triangle, with base length and height
             Triangle Double Double
           | ||| A rectangle, with length and height
             Rectangle Double Double
           | ||| A circle, with radius
             Circle Double

%name Shape shape, shape1, shape2

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius


data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

%name Picture pic, pic1, pic2


testPicture : Picture
testPicture = Combine (Translate 5 5 (Primitive (Rectangle 20 10))) 
                      (Combine (Translate 35 5 (Primitive (Circle 5))) 
                               (Translate 15 25 (Primitive (Triangle 10 10))))


pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic pic1) = pictureArea pic + pictureArea pic1
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic
