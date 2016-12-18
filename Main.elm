-- elm-lang/core
import Task
import Random
import Html exposing (..)
import Html.Attributes
import Html.Events

-- elm-lang/window
import Window exposing (..)


import Time exposing (..)

import Support exposing (..)
import Vector exposing (..)
import Model exposing (..)
import Steering exposing (..)
import ParseInt
import LotteryView exposing (lotteryView)

main =
    program 
      { init = init
      , view = view
      , update = update
      , subscriptions = subscriptions
      }

init : ( Model, Cmd Msg )
init = (newModel, Task.perform (\x -> Resize x) Window.size )

moveCow = Random.generate PlayLottery (Random.pair 
    (Random.float (-1) 1) (Random.float (-1) 1)
  )
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize newSize ->
            ( {model | size = newSize}, Cmd.none )
        Fail ->
            ( model, Cmd.none )
          
        StartLottery ->
            ( {model | 
                remainingTime = model.lotteryDuration, 
                winningLot = Nothing,
                runningLottery = True
              }, Cmd.none)
        PlayLottery (randomx, randomy) -> let
                newTarget = nextWanderTarget model.tpf model.cow.wanderTarget randomx randomy
                force = wander model.cow.position model.cow.velocity newTarget
                tpf = (Vector (model.tpf * model.tpf) (model.tpf * model.tpf))
                velocity = (Vector.truncate maxspeed 
                  (plus
                    (multiply 
                      tpf 
                      (divide 
                        force 
                        (Vector model.cow.mass model.cow.mass)
                      )
                    )
                    model.cow.velocity
                  )
                )
                newCowPosition = modular (worldDimensions model.count) (plus model.cow.position velocity) 
            in
              ( {model | 
                  cow = Cow
                    newCowPosition 
                    velocity
                    model.cow.mass
                    newTarget,
                  rng = (Vector randomx randomy)
                },  
                if model.remainingTime > 0 then Cmd.none else message SelectWinner
              )
        Tick _ ->
            ( {model | 
                remainingTime = model.remainingTime - 1.0
              }, 
              Cmd.none 
            )
        CowTick tpf ->
            ({model | tpf = ((tpf - model.lastTime)/1000), lastTime = tpf}, if model.runningLottery then moveCow else Cmd.none)
        SelectWinner ->
            let
                winner = (shitOnLot model.count model.cow)
                draftsleft = model.draftsLeft - 1
                newCow = Cow
                    model.cow.position
                    (divide model.cow.velocity (Vector 10 10))
                    model.cow.mass
                    model.rng
                tryAgainModel = {model | 
                  cow = newCow,
                  remainingTime = model.lotteryDuration * redoTimeFraction * (toFloat model.runAttempts),
                  runAttempts = model.runAttempts + 1
                }
            in
              if List.member winner model.passedWinners then
                (tryAgainModel, Cmd.none)
              else
                if winner.id > model.count then
                  ({tryAgainModel | 
                    passedWinners = (tryAgainModel.passedWinners ++ [winner])
                  }, Cmd.none)
                else
                  ({model | 
                      winningLot = Maybe.Just winner,
                      cow = newCow,
                      passedWinners = model.passedWinners ++ [winner],
                      runAttempts = 1,
                      runningLottery = False,
                      draftsLeft = draftsleft
                    }, Cmd.none)
        FormLotCount count ->
            parseInput (\model x -> {model | count = x}) model count
        FormDraftCount drafts ->
            parseInput (\model x -> {model | draftsLeft = x}) model drafts
        FormTime time ->
            parseInput (\model x -> {model | lotteryDuration = ((inSeconds ((toFloat x)* 1000.0)) )}) model time
        FormBegin ->
            ({model | showForm = False}, Cmd.none)
        StopLottery ->
            ({model | showForm = True}, Cmd.none)


parseInput: (Model -> Int -> Model) -> Model -> String -> ( Model, Cmd Msg )
parseInput func model input = Result.withDefault 
  (model, Cmd.none)
  (Result.map 
    (\x -> (func model x, Cmd.none)) (ParseInt.parseInt input))
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ 
      Window.resizes Resize,  
      Time.every second Tick,
      Time.every (Time.second / 20) CowTick
    ]

userput:  (String -> Msg) -> String -> String -> Html Msg
userput msg string value = tr [] [
                    td [] [
                        text string
                    ],
                    td [
                    ] [
                      input [
                        Html.Attributes.style [
                             ("width", "200px"),
                             ("padding", "10px")
                        ],
                        Html.Attributes.type_ "number", 
                        Html.Attributes.placeholder string, 
                        Html.Attributes.value (value),
                        Html.Events.onInput msg] [
                           
                        ]
                    ]
                  ]
view : Model -> Html Msg
view model =
    if (not model.showForm) then
        lotteryView model
    else
        div [
             Html.Attributes.style [
                  ("background", "green"),
                  ("width", "30%"),
                  ("position", "absolute"),
                  ("left", "35%"),
                  ("top", "35%"),
                  ("padding", "10px"),
                  ("border-radius", "10px"),
                  ("border", "3px solid darkgreen")]
        ] [
             table [] [
                  userput FormLotCount "Aantal loten" (toString model.count),
                  userput FormDraftCount "Aantal trekkingen" (toString model.draftsLeft),
                  userput FormTime "Tijd seconden" (toString model.lotteryDuration ),
                  tr [] [
                    td [
                        Html.Attributes.colspan 2
                    ] [
                      button [
                        Html.Events.onClick FormBegin
                      ] [
                         text "Begin"
                      ]
                    ]
                  ]
              ]
        ]
