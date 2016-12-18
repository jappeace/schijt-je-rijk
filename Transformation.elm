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
module Transformation exposing (..)
import Vector exposing (Vector)
import Matrix exposing (Matrix)

toWorld : Float -> Vector -> Vector -> Vector
toWorld agent_rotation agent_position target_position = 
  Matrix.transform
    (Matrix.multiply
      (Matrix.rotate agent_rotation)
      (Matrix.translate agent_position.x agent_position.y)) 
    target_position

-- a little more complix api but avoid sin/cos stuff
toWorldAround : Vector -> Vector -> Vector -> Vector -> Vector
toWorldAround agent_heading agent_side agent_position target_position = 
  Matrix.transform
    (Matrix.multiply
      (Matrix.rotateAround agent_heading agent_side)
      (Matrix.translate agent_position.x agent_position.y)
    ) 
    target_position
