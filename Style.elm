module Style exposing (..)

import Css exposing (..)
import Css.Namespace exposing (namespace)


type CssClasses
    = NavBar


type CssIds
    = LotteryInteraction 


sheetFile : String
sheetFile = "styles.css"

css:Stylesheet
css =
    (stylesheet << namespace "schijt")
    [
     (#) LotteryInteraction [
              position absolute,
              left (pct 40),
              top (pct 40),
              width (pct 20),
              height (pct 20),
              fontSize (pt 40),
              fontFamilies ["Comic", "Sans", ",", "Comic", "Sans", "MS"]
         ]
    ]
