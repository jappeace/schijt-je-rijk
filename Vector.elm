module Vector exposing (..)

type alias Vector = {x:Float, y:Float}
multiply : Vector -> Vector -> Vector
multiply a b = apply (*) a b
divide : Vector -> Vector -> Vector
divide a b = apply (/) a b

plus:Vector -> Vector -> Vector
plus a b = apply (+) a b
minus:Vector -> Vector -> Vector
minus a b = apply (-) a b

apply : (Float -> Float -> Float) -> Vector -> Vector -> Vector
apply f vec oth = Vector (f vec.x oth.x) (f vec.y oth.y)
applySingle : (Float -> Float) -> Vector -> Vector
applySingle f vec = Vector (f vec.x) (f vec.y)

modularf: Float -> Float -> Float
modularf val bound = if val < 0 
  then 
    val + bound 
  else (
    if val > bound 
      then 
        val - bound 
      else val
  )

-- wrap the vector around the given bounds
modular: Vector -> Vector -> Vector
modular bounds input = (apply (modularf) input bounds)

lengthSq: Vector -> Float
lengthSq vector = vector.x * vector.x + vector.y * vector.y
length: Vector -> Float
length vector = sqrt (lengthSq vector)

normalize: Vector -> Vector
normalize vector = let
      len = length vector
    in
      Vector ((vector.x)/len) ((vector.y)/len)

perpendicular: Vector -> Vector
perpendicular vector = Vector (-vector.y) vector.x

angle: Vector -> Float
angle vec = (atan2 vec.y vec.x)

truncate: Float -> Vector -> Vector
truncate max vector = if (length vector) > max 
    then
      multiply (normalize vector) (Vector max max)
    else
      vector
