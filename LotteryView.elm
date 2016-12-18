module LotteryView exposing (lotteryView)
-- evancz/elm-graphics
import Collage exposing (collage, gradient, rect, Form)
import Element exposing (toHtml)
import Transform

import Html exposing (..)
import Html.Attributes
import Html.Events

import Color exposing (linear, rgb)
import Text

import Support exposing (..)
import Vector exposing (..)
import Model exposing (..)
import Steering exposing (..)


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

lotteryView : Model -> Html Msg
lotteryView model =
    let
        visibility = if model.runningLottery then "none" else "block"
    in
      div [Html.Attributes.id "lottery"] [
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
        ]
      ]
  
