module Steering exposing (wander, nextWanderTarget)
import Vector exposing (..)
import Matrix exposing (..)
import Model exposing (..)
import Transformation exposing (..)

wanderRadius = Vector 4 4
wanderTargetDistance = Vector 0 0

jitter = 2
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
    heading = normalize force
  in
    (minus (toWorldAround heading (perpendicular heading) position target) position)
