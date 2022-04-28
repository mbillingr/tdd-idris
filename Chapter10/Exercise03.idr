import Chapter10.DataStore

-- Exercise 1

getValues : DataStore (SString .+. val_schema) -> List (SchemaType val_schema)
getValues input with (storeView input)
  getValues _ | SNil = []
  getValues _ | (SAdd (key, value) store rec) = value :: getValues _ | rec



testStore : DataStore (SString .+. SInt)
testStore = addToStore ("First", 1) $ 
            addToStore ("Second", 2) $ 
            addToStore ("Third", 3) $ 
            empty

-- Exercise 2

export
data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

export
triangle : Double -> Double -> Shape
triangle = Triangle

export
rectangle : Double -> Double -> Shape
rectangle = Rectangle

export circle : Double -> Shape
circle = Circle


public export
data ShapeView : Shape -> Type where
     STriangle : ShapeView (triangle b h)
     SRectangle : ShapeView (rectangle w h)
     SCircle : ShapeView (circle r)

shapeView : (input : Shape) -> ShapeView input
shapeView (Triangle x y) = STriangle
shapeView (Rectangle x y) = SRectangle
shapeView (Circle x) = SCircle

area : Shape -> Double
area s with (shapeView s)
  area (triangle b h) | STriangle = 0.5 * b * h
  area (rectangle w h) | SRectangle = w * h
  area (circle r) | SCircle = pi * r * r
