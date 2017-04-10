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
        background = Collage.toForm (Element.image width height "img/background.png")
        backgroundColor = Collage.filled Color.darkGreen (Collage.rect (toFloat width) (toFloat height))
    in
      toHtml (collage width height (
          [
          backgroundColor,
          Collage.groupTransform cowTransform [background],
          lots,
          shits,
          (Collage.rotate 
            (cowAngle model.cow.velocity) 
            (Collage.rotate (degrees cowInitialRotation)
            (Collage.toForm 
              (Element.image cowSize cowSize "img/cow.png")
            )))
          ]
        )
      )

msgBox visibility elements = div [
            Html.Attributes.style [
              ("position", "absolute"), 
              ("left", "40%"), 
              ("top", "10%"), 
              ("width", "20%"),
              ("font-size", "40pt"),
              ("color", "white"),
              ("background", "green"),
              ("padding", "10px"),
              ("border-radius", "10px"),
              ("border", "3px solid darkgreen"),
              ("text-align", "center"),
              ("display", visibility)
            ]
          ] elements
actionButton msg action = img [
              Html.Events.onClick (action),
              Html.Attributes.style [
                ("width", "100%"),
                ("height", "100%"),
                ("font-size", "inherit"),
                ("font-family", "inherit")
              ],
              Html.Attributes.src "img/button.png"
            ] [text msg]
lotteryView : Model -> Html Msg
lotteryView model =
    let
        winnertxt = text (Maybe.withDefault "" (Maybe.map (\x -> (toString x.id) ++ " wint") model.winningLot))
        visibility = if model.runningLottery then "none" else "block"
        someButton = if model.draftsLeft < 1 then (
          actionButton "Einde!" StopLottery
        ) else (
          actionButton "Begin trekking!" StartLottery
        )
    in
      div [Html.Attributes.id "lottery"] [
        render model,
        msgBox visibility [
          someButton,
          winnertxt
        ]
      ]

