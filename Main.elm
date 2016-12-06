-- elm-lang/core
import Task
import Color exposing (linear, rgb)
import Random
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events

-- evancz/elm-graphics
import Collage exposing (collage, gradient, rect, Form)
import Element exposing (toHtml)
import Transform

-- elm-lang/window
import Window exposing (..)

import Text

import Style
import Html.CssHelpers

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
      count : Int ,
      inLottery : Bool,
      winningLot : Maybe Int
    }

init : ( Model, Cmd Msg )
init =
    (Model (Window.Size 0 0) 4 False Nothing, Task.perform (\x -> Resize x) Window.size )


type Msg
    = Resize Window.Size
    | Fail
    | StartLottery
    | SelectWinningLot Int

type alias Lot = {id:Int, x:Int, y:Int}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize newSize ->
            ( {model | size = newSize}, Cmd.none )
        Fail ->
            ( model, Cmd.none )
        StartLottery ->
            ( {model | inLottery = True},  Random.generate SelectWinningLot (Random.int 1 model.count))
        SelectWinningLot selected ->
            ( {model | winningLot = Maybe.Just selected}, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Window.resizes Resize ]

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
        scale = toFloat ((calcDimension model.count))
        location = (
          ((toFloat model.size.width) / scale) * ((toFloat lot.x)),
          ((toFloat model.size.height) / scale) * (toFloat lot.y) 
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


{ id, class, classList } = Html.CssHelpers.withNamespace "schijt"
view : Model -> Html Msg
view model =
    let
        visibility = if model.inLottery then "hidden" else "visible"
    in
      body [] [
        node "link" [
          attribute "rel" "stylesheet",
          attribute "property" "stylesheet",
          attribute "href" Style.sheetFile
        ] [],
        h1 [] [text ("Schijt je rijk" )], 
        render model,
        div [
          id Style.LotteryInteraction,
          style [
            ("visibility", visibility)
          ]
        ] [
          button [
            Html.Events.onClick StartLottery
          ] [text "Begin trekking!"],
          text (Maybe.withDefault "" (Maybe.map toString model.winningLot))
        ]
      ]
  
