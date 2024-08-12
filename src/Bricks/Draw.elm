module Bricks.Draw exposing (..)

import Array exposing (Array)
import Array2D exposing (Array2D)
import Bricks.Config as Config
import Bricks.Types exposing (..)
import Element exposing (Element)
import Point exposing (Point)
import Svg exposing (Svg)
import Svg.Attributes
import SvgHelpers exposing (colorToSvgString, drawToPointString, moveToPointString, pointToString)


view : Model -> Element msg
view model =
    Element.el [ Element.explain Debug.todo ] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.height <| String.fromFloat <| 800.0
                ]
                [ Svg.defs
                    []
                    []
                , draw model
                ]


draw : Model -> Svg msg
draw model =
    let
        drawnBricks =
            model.bricks
                |> Array2D.map drawBrick
                |> Array2D.toFlatArrayRowMajor
                |> Array.toList
    in
    Svg.g [] drawnBricks


drawBrick : Brick -> Svg msg
drawBrick brick =
    let
        position =
            brick.homePoint

        transformString =
            String.join " "
                [ SvgHelpers.rotateString (position.y * 0.005)
                ]
    in
    Svg.rect
        [ Svg.Attributes.x <| String.fromFloat position.x
        , Svg.Attributes.y <| String.fromFloat position.y
        , Svg.Attributes.class "brickrect"
        , Svg.Attributes.width <| String.fromInt Config.brickWidth
        , Svg.Attributes.height <| String.fromInt Config.brickHeight
        , Svg.Attributes.transform transformString
        , Svg.Attributes.fill <| colorToSvgString Config.brickFillColor
        , Svg.Attributes.stroke <| colorToSvgString Config.brickStrokeColor
        , Svg.Attributes.strokeWidth "1"
        ]
        []
