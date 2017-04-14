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
module Model exposing (..)

import Support exposing (..)
import Vector exposing (..)
import Time exposing (Time, inSeconds)
import Window exposing (Size)
import Transformation exposing (toWorld)
-- Model

type Msg
    = Resize Window.Size
    | Fail
    | StartLottery
    | PlayLottery (Float, Float)
    | Tick Time
    | SelectWinner
    | CowTick Time
    | FormLotCount String
    | FormDraftCount String
    | FormTime String
    | FormBegin
    | StopLottery

type alias Model = 
    { 
      size : Window.Size,
      count : Int ,
      remainingTime: Time,
      timeRange: (Time, Time),
      lotteryDuration: Time,
      winningLot : Maybe Lot,
      cow : Cow,
      lastTime: Time,
      tpf: Time,
      rng: Vector,
      runningLottery:Bool,
      passedWinners: List Lot,
      runAttempts:Int,
      draftsLeft:Int,
      showForm:Bool
    }
type alias Cow =
    {
        position : Vector,
        velocity: Vector,
        mass: Float,
        wanderTarget: Vector 
    }

maxspeed = 6.0
tileScale = Vector 3 3
tileSize = Vector 80 80
cowSize = 100
someVector = Vector 0.2 0.2
mass = 0.6
redoTimeFraction = 0.25 -- if we try to select an already won lot, how much extra time to move away? (as a fraction of the inputed time)

cowInitialRotation = 60 -- degrees

newModel:Model
newModel = Model 
      (Window.Size 0 0) 
      0 
      (inSeconds 0)
      (inSeconds 0, inSeconds 0)
      (inSeconds 0)
      Nothing
      (Cow (Vector 20.0 20.0) someVector mass someVector)
      0.0
      0.0
      (Vector 0.1 0.0)
      False
      []
      1
      0
      True
    
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
