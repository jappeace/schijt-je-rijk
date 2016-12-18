module Steering exposing (wander, nextWanderTarget)
import Vector exposing (..)
import Matrix exposing (..)
import Model exposing (..)
import Transformation exposing (..)

wanderRadius = Vector 3 3
wanderTargetDistance = Vector 10 0

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
