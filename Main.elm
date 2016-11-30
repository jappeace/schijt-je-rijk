-- elm-lang/core
import Task
import Color exposing (linear, rgb)

-- elm-lang/html
import Html exposing (..)

-- evancz/elm-graphics
import Collage exposing (collage, gradient, rect, Form)
import Element exposing (toHtml)
import Transform

-- elm-lang/window
import Window exposing (..)

import Text
--join : List (List a) -> List a
join =
  List.foldr (++) []


{-|-}
--flatMap : (a -> List b) -> List a -> List b
flatMap f list =
  List.map f list
    |> join


{-|-}
flatMap2 : (a -> b -> List c) -> List a -> List b -> List c
flatMap2 f list1 list2 =
  List.map2 f list1 list2
    |> join

main =
    program 
      { init = init
      , view = view
      , update = update
      , subscriptions = subscriptions
      }

-- Model

type alias Model = 
    { size : Window.Size,
      count : Int 
    }

init : ( Model, Cmd Msg )
init =
    (Model (Window.Size 0 0) 40, Task.perform (\x -> Resize x) Window.size )


type Msg
    = Resize Window.Size
    | Fail

type alias Lot = {id:Int, x:Int, y:Int}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize newSize ->
            ( {model | size = newSize}, Cmd.none )
        Fail ->
            ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Window.resizes Resize ]

calcDimension : Int -> Int
calcDimension amount = (ceiling (sqrt (toFloat amount)))
createLots : Int -> List Lot
createLots amount = 
  let 
    scale = calcDimension amount
    ranges = List.range 0 scale
  in
      flatMap (\row -> List.map (\col -> Lot (row * (scale+1) + col) row col) ranges ) ranges

renderLot : Lot -> Form
renderLot lot = 
    let
        location = (toFloat lot.x, toFloat lot.y)
    in
      Collage.move location (Collage.text (Text.fromString (toString lot.id)))

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
              (Transform.scaleX ((toFloat width) / (toFloat model.count)))
              (Transform.scaleY ((toFloat height) / (toFloat model.count)))
            )
            (List.filterMap 
              (\lot -> 
                (
                  if lot.id > model.count then 
                    Maybe.Nothing
                  else 
                    Maybe.Just (renderLot lot))
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
      count = 40
  in
      body [] [
        h1 [] [text "Schijt je rijk"], 
        render model
      ]
  
