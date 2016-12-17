module Steering exposing (wander, nextWanderTarget)
import Vector exposing (..)
import Matrix exposing (..)
import Model exposing (..)
import Transformation exposing (toWorld)

wanderRadius = Vector 3 3
wanderTargetDistance = Vector 5 0

jitter = 1
nextWanderTarget: Float -> Vector -> Float -> Float -> Vector
nextWanderTarget time currentTarget rx ry = Vector.multiply
  wanderRadius
  (normalize (plus 
    (Vector (rx*jitter*time) (ry*jitter*time)) 
    (currentTarget)
  ))
wander: Vector -> Vector -> Vector -> Vector
wander position force newTarget = let
    target = plus newTarget wanderTargetDistance
  in
    (minus (toWorld (angle force) position target) position )
