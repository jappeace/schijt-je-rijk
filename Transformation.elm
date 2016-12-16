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
      (Matrix.translate agent_position.x agent_position.y)) 
    target_position
