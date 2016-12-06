port module CompileCss exposing (..)

import Css.File exposing (..)
import Style
import Html exposing (div)

cssFiles : CssFileStructure
cssFiles =
    toFileStructure [ ( Style.sheetFile, compile [ Style.css ] ) ]

port files : CssFileStructure -> Cmd msg

main : Program Never () msg
main =
    Html.program
        { init = ( (), files cssFiles )
        , view = \_ -> (div [] [])
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
