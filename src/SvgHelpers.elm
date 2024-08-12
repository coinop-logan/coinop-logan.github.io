module SvgHelpers exposing (..)

import CommonTypes exposing (..)
import Element exposing (Element)
import Point exposing (Point)
import Utils


pointToString : Point -> String
pointToString p =
    String.fromFloat p.x ++ "," ++ String.fromFloat p.y


rgbToSvgString : RGB -> String
rgbToSvgString rgb =
    let
        numbersString =
            [ rgb.red
            , rgb.green
            , rgb.blue
            ]
                |> List.map
                    (\f ->
                        f * 255 |> floor
                    )
                |> List.map String.fromInt
                |> String.join " "
    in
    "rgb(" ++ numbersString ++ ")"


colorToSvgString : Element.Color -> String
colorToSvgString =
    Utils.elementColorToRgb >> rgbToSvgString


boolToInt : Bool -> Int
boolToInt flag =
    if flag then
        1

    else
        0


moveToPointString : Point -> String
moveToPointString point =
    "M " ++ pointToString point


drawToPointString : Point -> String
drawToPointString point =
    "L " ++ pointToString point


translateString : Point -> String
translateString point =
    "translate(" ++ String.fromFloat point.x ++ "," ++ String.fromFloat point.y ++ ")"


rotateString : Float -> String
rotateString radians =
    let
        degrees =
            radians / pi * 180
    in
    "rotate(" ++ String.fromFloat degrees ++ ")"
