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


rgbToElementColor : RGB -> Element.Color
rgbToElementColor rgb =
    Element.rgb rgb.red rgb.blue rgb.green


interpolateFloat : Float -> Float -> Float -> Float
interpolateFloat f a b =
    (b - a) * f + a
