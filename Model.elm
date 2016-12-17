module Model exposing (..)

import Support exposing (..)
import Vector exposing (..)
import Time exposing (Time, inSeconds)
import Window exposing (Size)
import Transformation exposing (toWorld)
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
      runningLottery:Bool,
      passedWinners: List Lot,
      runAttempts:Int
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
redoTimeFraction = 0.25 -- if we try to select an already won lot, how much extra time to move away? (as a fraction of the inputed time)

newModel:Model
newModel = Model 
      (Window.Size 0 0) 
      100 
      (inSeconds 0)
      (inSeconds 2000)
      Nothing
      (Cow (Vector 20.0 20.0) someVector mass someVector)
      0.0
      0.0
      (Vector 0.1 0.0)
      False
      []
      1
    
type alias Lot = {id:Int, x:Int, y:Int}

screenTileSize = multiply tileSize tileScale

worldDimensions: Int -> Vector
worldDimensions lotCount =
  let 
    scale = toFloat (calcDimension lotCount)
  in 
    multiply tileSize (Vector scale scale)
shitOnLot : Int -> Cow -> Lot
shitOnLot lotCount cow = 
  let
    worldDimSize = (calcDimension lotCount + 1)
    lotCoords = applySingle (toFloat << floor) (divide (Vector (cow.position.x + (tileSize.x*0.8)) (cow.position.y+tileSize.y + (cowSize* 0.4))) tileSize)
  in
    Lot (worldDimSize * (floor lotCoords.x) + (floor lotCoords.y)) (floor lotCoords.x) (floor lotCoords.y)

cowAngle: Vector -> Float
cowAngle velocity = (angle velocity) + pi

calcDimension : Int -> Int
calcDimension amount = (ceiling (sqrt (toFloat amount))) - 1
createLots : Int -> List Lot
createLots amount = 
  let 
    scale = calcDimension amount
    ranges = List.range 0 scale
  in
      flatMap (\row -> List.map (\col -> Lot (row * (scale+1) + col+1) row col) ranges ) ranges
