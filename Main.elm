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
main =
    program 
      { init = init
      , view = view
      , update = update
      , subscriptions = subscriptions
      }

-- Model

type alias Model = 
    { 
      size : Window.Size,
      count : Int ,
      remainingTime: Time,
      lotteryDuration: Time,
      winningLot : Maybe Int,
      cow : Cow
    }
type alias Cow =
    {
        speed : Float, -- tile/s
        position : Vector,
        rotation: Float
    }
type alias Vector = {x:Float, y:Float}
tileSize = Vector 50 50

init : ( Model, Cmd Msg )
init =
    (
    (Model 
      (Window.Size 0 0) 
      4 
      (inSeconds 0)
      (inSeconds 10)
      Nothing
      (Cow 0.5 (Vector 20.0 20.0) 0.3)
    ), 
    Task.perform (\x -> Resize x) Window.size )


type Msg
    = Resize Window.Size
    | Fail
    | StartLottery
    | PlayLottery (Float, Float)
    | Tick Time
    | SelectWinner

type alias Lot = {id:Int, x:Int, y:Int}
moveCow = Random.generate PlayLottery (Random.pair 
    (Random.float 0 1) (Random.float 0 1)
  )
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize newSize ->
            ( {model | size = newSize}, Cmd.none )
        Fail ->
            ( model, Cmd.none )
          
        StartLottery ->
            ( {model | remainingTime = model.lotteryDuration, winningLot = Nothing}, moveCow)
        PlayLottery (randomx, randomy) -> let
                newCowPosition = (Vector 
                    (model.cow.position.x + randomx) 
                    (model.cow.position.y + randomy)
                  ) 
                nextTask = if model.remainingTime > 0 then moveCow else message SelectWinner
            in
              ( {model | 
                  cow = Cow 
                    model.cow.speed 
                    newCowPosition 
                    model.cow.rotation
                },  
                  nextTask
              )
        Tick _ ->
            ( {model | 
                remainingTime = model.remainingTime - 1.0
              }, 
              Cmd.none 
            )
        SelectWinner ->
            ({model | 
                winningLot = Maybe.Just 1 -- TODO calulate where the cow's at and return that id
              }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ 
      Window.resizes Resize,  
      Time.every second Tick
    ]

calcDimension : Int -> Int
calcDimension amount = (ceiling (sqrt (toFloat amount))) - 1
createLots : Int -> List Lot
createLots amount = 
  let 
    scale = calcDimension amount
    ranges = List.range 0 scale
  in
      flatMap (\row -> List.map (\col -> Lot (row * (scale+1) + col+1) row col) ranges ) ranges

renderLot : Model -> Lot -> Form
renderLot model lot = 
    let
        location = (
          tileSize.x * ((toFloat lot.x)),
          tileSize.y * (toFloat lot.y) 
        )
          
    in
      Collage.move location
      (Collage.text (Text.fromString (toString lot.id)))

render : Model -> Html Msg
render model = 
    let
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
        lots = 
          Collage.groupTransform
          (Transform.multiply
            (Transform.scale 0.8)
            (Transform.translation ((toFloat (-width))/2) ((toFloat (-height))/2))
          )
          (List.filterMap 
              (\lot -> 
                (
                  if lot.id > model.count then 
                    Maybe.Nothing
                  else 
                    Maybe.Just (renderLot model lot))
              ) 
              (createLots model.count)
          )
    in
        toHtml (collage width height (
            [gradient (linear (0, 0) (toFloat width, toFloat height) clrStops) (rect (toFloat width) (toFloat height)),
            Collage.move (100.0, 300.0) (gradient (linear ( 200, 0 ) (toFloat width, toFloat height) (List.reverse clrStops)) (rect (toFloat width/2) (toFloat height-500)))
            ] ++
            [lots]
          )
        )

view : Model -> Html Msg
view model =
    let
        visibility = if model.remainingTime > 0 then "none" else "block"
    in
      body [] [
        h1 [] [text ("Schijt je rijk" ++ (toString model.remainingTime))], 
        render model,
        div [
          Html.Attributes.style [
            ("position", "absolute"), 
            ("left", "40%"), 
            ("top", "40%"), 
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
          text (Maybe.withDefault "" (Maybe.map (\x -> "de winnaar is " ++ (toString x)) model.winningLot))
        ]
      ]
  
