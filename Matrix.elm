{-
This is a lottery game called "schijt je rijk".
Copyright (C) 2016 Jappie Klooster

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.If not, see <http://www.gnu.org/licenses/>.
-}
module Matrix exposing (..)
import Vector exposing (Vector)
type alias Matrix = {
            a1:Float, a2:Float, a3:Float, 
            b1:Float, b2:Float, b3:Float,
            c1:Float, c2:Float, c3:Float
}

multiply: Matrix -> Matrix -> Matrix
multiply x y = Matrix 
-- first row
  ((x.a1*y.a1) + (x.a2*y.b1) + (x.a3*y.c1))
  ((x.a1*y.a2) + (x.a2*y.b2) + (x.a3*y.c2))
  ((x.a1*y.a3) + (x.a2*y.b3) + (x.a3*y.c3))
-- second
  ((x.b1*y.a1) + (x.b2*y.b1) + (x.b3*y.c1))
  ((x.b1*y.a2) + (x.b2*y.b2) + (x.b3*y.c2))
  ((x.b1*y.a3) + (x.b2*y.b3) + (x.b3*y.c3))
-- third
  ((x.c1*y.a1) + (x.c2*y.b1) + (x.c3*y.c1))
  ((x.c1*y.a2) + (x.c2*y.b2) + (x.c3*y.c2))
  ((x.c1*y.a3) + (x.c2*y.b3) + (x.c3*y.c3))

transform: Matrix -> Vector -> Vector
transform matrix vector = Vector
  ((matrix.a1*vector.x) + (matrix.b1*vector.y) + (matrix.c1))
  ((matrix.a2*vector.x) + (matrix.b2*vector.y) + (matrix.c2))

identity : Matrix
identity = Matrix 
  1 0 0
  0 1 0
  0 0 1

translate: Float -> Float -> Matrix
translate x y = Matrix
 1 0 0
 0 1 0
 x y 1
scale: Float -> Float -> Matrix
scale x y = Matrix 
 x 0 0
 0 y 0
 0 0 1

rotate: Float -> Matrix
rotate rad = let
    s = sin rad
    c = cos rad
  in 
    Matrix
  c    s 0
  (-s) c 0
  0    0 1

rotateAround: Vector -> Vector -> Matrix
rotateAround heading side = Matrix
  heading.x heading.y 0
  side.x    side.y    0
  0         0         1
