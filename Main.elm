-- elm-lang/core
import Task
import Color exposing (linear, rgb)

-- elm-lang/html
import Html exposing (..)

-- evancz/elm-graphics
import Collage exposing (collage, gradient, rect)
import Element exposing (toHtml)

-- elm-lang/window
import Window exposing (..)


main =
    program 
      { init = init
      , view = view
      , update = update
      , subscriptions = subscriptions
      }

-- Model

type alias Model = 
    { size : Window.Size
    }

init : ( Model, Cmd Msg )
init =
    ( Model ( Window.Size 0 0 ), Task.perform (\x -> Resize x) Window.size )


type Msg
    = Resize Window.Size
    | Fail

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize newSize ->
            ( Model newSize, Cmd.none )
        Fail ->
            ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Window.resizes Resize ]


view : Model -> Html Msg
view model =
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

        gfx =
            collage width height
                [ gradient (linear ( 0, 0 ) (toFloat width, toFloat height) clrStops) (rect (toFloat width) (toFloat height)),
                Collage.move (100.0, 300.0) (gradient (linear ( 200, 0 ) (toFloat width, toFloat height) (List.reverse clrStops)) (rect (toFloat width/2) (toFloat height-500)))
                ]
    in
  body [] [
    h1 [] [text "Schijt je rijk"], 
    toHtml gfx
  ]
  
