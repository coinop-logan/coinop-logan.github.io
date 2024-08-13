module Bricks.Draw exposing (..)

import Bricks.Brick exposing (Brick)
import Bricks.Config as Config
import Bricks.Convert as Convert
import Bricks.Types exposing (..)
import Bricks.Wall as BrickWall exposing (BrickWall)
import Element exposing (Element)
import Point exposing (Point)
import Svg exposing (Svg)
import Svg.Attributes
import SvgHelpers exposing (colorToSvgString, drawToPointString, moveToPointString, pointToString)
import Time


view : Time.Posix -> Model -> Element msg
view now model =
    Element.el [ Element.explain Debug.todo ] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.height <| String.fromFloat <| 800.0
                , Svg.Attributes.width <| String.fromFloat <| 800.0
                ]
                [ Svg.defs
                    []
                    []
                , draw now model
                ]


draw : Time.Posix -> Model -> Svg msg
draw now model =
    let
        drawnBricks =
            model.bricks
                |> BrickWall.toList
                |> List.map (drawBrick now)
    in
    Svg.g [] drawnBricks


drawBrick : Time.Posix -> Brick -> Svg msg
drawBrick now brick =
    let
        ( position, rotation ) =
            Convert.getBrickPosAndRot now brick

        transformString =
            String.join " "
                [ SvgHelpers.rotateString rotation
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
