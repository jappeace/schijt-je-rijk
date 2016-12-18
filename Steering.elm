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
module Steering exposing (wander, nextWanderTarget)
import Vector exposing (..)
import Matrix exposing (..)
import Model exposing (..)
import Transformation exposing (..)

wanderRadius = Vector 3 3
wanderTargetDistance = Vector 3 0

jitter = 3
nextWanderTarget: Float -> Vector -> Float -> Float -> Vector
nextWanderTarget time currentTarget rx ry = Vector.multiply
  wanderRadius
  (normalize (plus 
    (Vector (rx*jitter*time) (ry*jitter*time)) 
    (currentTarget)
  ))
wander: Vector -> Vector -> Vector -> Vector
wander position velocity newTarget = let
    target = plus newTarget wanderTargetDistance
    heading = normalize velocity
  in
    (minus (toWorld (angle velocity) position target) position)
