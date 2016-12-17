-- elm-lang/core
import Task
import Color exposing (linear, rgb)
import Random
import Html exposing (..)
import Html.Attributes
import Html.Events
-- evancz/elm-graphics
import Collage exposing (collage, gradient, rect, Form)
import Element exposing (toHtml)
import Transform

-- elm-lang/window
import Window exposing (..)

import Text

import Time exposing (..)

import Support exposing (..)
import Vector exposing (..)
import Model exposing (..)
import Steering exposing (..)

main =
    program 
      { init = init
      , view = view
      , update = update
      , subscriptions = subscriptions
      }

init : ( Model, Cmd Msg )
init = (newModel, Task.perform (\x -> Resize x) Window.size )

type Msg
    = Resize Window.Size
    | Fail
    | StartLottery
    | PlayLottery (Float, Float)
    | Tick Time
    | SelectWinner
    | CowTick Time

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
                newCow = Cow
                    model.cow.position
                    model.rng
                    model.cow.mass
                    model.cow.wanderTarget
            in
              if List.member winner model.passedWinners then
                ({model | 
                  cow = newCow,
                  remainingTime = model.lotteryDuration * redoTimeFraction * (toFloat model.runAttempts),
                  runAttempts = model.runAttempts + 1
                }, Cmd.none)
              else
                ({model | 
                    winningLot = Maybe.Just winner,
                    cow = newCow,
                    passedWinners = model.passedWinners ++ [winner],
                    runAttempts = 1,
                    runningLottery = False
                  }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ 
      Window.resizes Resize,  
      Time.every second Tick,
      Time.every (Time.second / 20) CowTick
    ]

renderLot : Form -> Lot -> Form
renderLot withShape lot = 
    let
        location = (
          tileSize.x * ((toFloat lot.x)),
          tileSize.y * (toFloat lot.y) 
        )
          
    in
      Collage.scale 3
      (Collage.move location withShape)

render : Model -> Html Msg
render model = let
        width =
            model.size.width

        height =
            model.size.height

        clrStart =
            rgb 5 250 140

        clrEnd =
            rgb 231 59 87

        clrStops =
            [ ( 0.0, clrStart ), ( 1.0, clrEnd ) ]
        -- move in oposite direction of cow
        cowTransform = (Transform.multiply
            (Transform.scale 1)
            (Transform.translation -model.cow.position.x -model.cow.position.y)
          )
        lots = 
          Collage.groupTransform
            cowTransform
              (List.filterMap 
                (\lot -> 
                  (if lot.id > model.count then 
                      Maybe.Nothing
                  else 
                    Maybe.Just (renderLot 
                      (Collage.text (Text.fromString (toString lot.id)))
                      lot 
                    )
                  )
                )
              (createLots model.count)
              ) 
        shits = Collage.groupTransform cowTransform
                (List.map (renderLot (Collage.moveY (-tileSize.y) (Collage.toForm (Element.image (floor (tileSize.x/2)) (floor (tileSize.y/2)) "img/shit.png"))))
                     model.passedWinners
                )
    in
      toHtml (collage width height (
          [gradient (linear (0, 0) (toFloat width, toFloat height) clrStops) (rect (toFloat width) (toFloat height)),
          Collage.move (100.0, 300.0) (gradient (linear ( 200, 0 ) (toFloat width, toFloat height) (List.reverse clrStops)) (rect (toFloat width/2) (toFloat height-500))),
          lots,
          shits,
          (Collage.rotate 
            (cowAngle model.cow.velocity) 
            (Collage.toForm 
              (Element.image cowSize cowSize "img/cow.png")
            ))
          ]
        )
      )

view : Model -> Html Msg
view model =
    let
        visibility = if model.runningLottery then "none" else "block"
    in
      body [] [
        render model,
        div [
          Html.Attributes.style [
            ("position", "absolute"), 
            ("left", "40%"), 
            ("top", "20%"), 
            ("width", "20%"),
            ("height", "20%"),
            ("font-size", "40pt"),
            ("font-family", "Comic Sans, Comic Sans MS")
          ]
        ] [
          button [
            Html.Events.onClick (StartLottery),
            Html.Attributes.style [
              ("width", "100%"),
              ("height", "100%"),
              ("display", visibility),
              ("font-size", "inherit"),
              ("font-family", "inherit")
            ]
          ] [text "Begin trekking!"],
          text (Maybe.withDefault "" (Maybe.map (\x -> "de winnaar is " ++ (toString x.id)) model.winningLot))
        ],
        h1 [] [text ("Schijt je rijk" ++ (toString model.remainingTime) ++ " -- cow pos" ++ (toString model.cow.position))],
        h1 [] [text ("force " ++ (toString model.cow.velocity))],
        h1 [] [text ("angle " ++ (toString (((angle model.cow.velocity)*180) / pi)))],
        h1 [] [text ("target" ++ (toString model.cow.wanderTarget))],
        h1 [] [text ("time " ++ (toString model.tpf))],
        h1 [] [text ("rng" ++ (toString model.rng))]
      ]
  
