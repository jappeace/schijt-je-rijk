module Model exposing (..)

import Support exposing (..)
import Vector exposing (..)
import Time exposing (Time, inSeconds)
import Window exposing (Size)
-- Model

type alias Model = 
    { 
      size : Window.Size,
      count : Int ,
      remainingTime: Time,
      lotteryDuration: Time,
      winningLot : Maybe Lot,
      cow : Cow,
      lastTime: Time,
      tpf: Time,
      rng: Vector,
      runningLottery:Bool
    }
type alias Cow =
    {
        position : Vector,
        velocity: Vector,
        mass: Float,
        wanderTarget: Vector 
    }

maxspeed = 12.0
tileScale = Vector 3 3
tileSize = Vector 80 80
cowSize = 100
someVector = Vector 0.2 0.2
mass = 0.6

newModel:Model
newModel = Model 
      (Window.Size 0 0) 
      100 
      (inSeconds 0)
      (inSeconds 20000)
      Nothing
      (Cow (Vector 20.0 20.0) someVector mass someVector)
      0.0
      0.0
      (Vector 0.0 0.0)
      False
    
type alias Lot = {id:Int, x:Int, y:Int}

screenTileSize = multiply tileSize tileScale

worldDimensions: Int -> Vector
worldDimensions lotCount =
  let 
    scale = toFloat (calcDimension lotCount)
  in 
    multiply tileSize (Vector scale scale)
cowposToLot : Int -> Vector -> Lot
cowposToLot lotCount cowpos = 
  let
    worldDimSize = (calcDimension lotCount + 1)
    lotCoords = applySingle (toFloat << floor) (divide (Vector (cowpos.x + (cowSize * 0.4) + tileSize.x) (cowpos.y+tileSize.y + (cowSize* 0.4))) tileSize)
  in
    Lot (worldDimSize * (floor lotCoords.x) + (floor lotCoords.y)) (floor lotCoords.x) (floor lotCoords.y)

calcDimension : Int -> Int
calcDimension amount = (ceiling (sqrt (toFloat amount))) - 1
createLots : Int -> List Lot
createLots amount = 
  let 
    scale = calcDimension amount
    ranges = List.range 0 scale
  in
      flatMap (\row -> List.map (\col -> Lot (row * (scale+1) + col+1) row col) ranges ) ranges
