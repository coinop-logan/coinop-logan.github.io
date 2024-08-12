module Utils exposing (..)

import CommonTypes exposing (..)
import Element exposing (Element)
import Types exposing (..)


elementColorToRgb : Element.Color -> RGB
elementColorToRgb elColor =
    let
        rgba =
            Element.toRgb elColor
    in
    RGB
        rgba.red
        rgba.green
        rgba.blue
