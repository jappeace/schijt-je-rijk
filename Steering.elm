module Steering exposing (wander, nextWanderTarget)
import Vector exposing (..)
import Matrix exposing (..)
import Model exposing (..)
import Transformation exposing (toWorldAround)

wanderRadius = Vector 5 5
wanderTargetDistance = Vector (cowSize/16) 0

jitter = 1
nextWanderTarget: Cow -> Float -> Float -> Vector
nextWanderTarget cow rx ry = Vector.multiply
  wanderRadius
  (normalize (plus 
    (Vector (rx*jitter) (ry*jitter)) 
    cow.wanderTarget
  ))
wander: Cow -> Float -> Float -> Vector
wander cow rx ry = let
    target = plus (nextWanderTarget cow rx ry) wanderTargetDistance
    heading = (normalize cow.force)  
  in
    toWorldAround heading (perpendicular heading) cow.position target
