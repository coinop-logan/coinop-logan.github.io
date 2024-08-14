module Utils exposing (..)

import CommonTypes exposing (..)
import Element exposing (Element)


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


interpolateFloat : Float -> Float -> Float -> Float
interpolateFloat f a b =
    (b - a) * f + a
