module BrickWall.Draw exposing (..)

import BrickWall.Brick as Brick exposing (Brick)
import BrickWall.BrickWall as BrickWall exposing (BrickWall)
import BrickWall.BricksContainer as BricksContainer
import BrickWall.Config as Config
import Element exposing (Element)
import Maybe.Extra as Maybe
import Point exposing (Point)
import Svg exposing (Svg)
import Svg.Attributes
import SvgHelpers exposing (colorToSvgString, drawToPointString, moveToPointString, pointToString)
import Time


view : Time.Posix -> Float -> Float -> BrickWall -> Element msg
view now width height model =
    Element.el [] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.width <| String.fromFloat width
                , Svg.Attributes.height <| String.fromFloat height
                ]
                [ Svg.defs
                    []
                    []
                , draw now model
                ]


draw : Time.Posix -> BrickWall -> Svg msg
draw now brickWall =
    let
        drawnBricks =
            brickWall.bricks
                |> BricksContainer.toList
                |> List.map (Maybe.map (drawBrick now))
                |> Maybe.values
    in
    Svg.g [] drawnBricks


drawBrick : Time.Posix -> Brick -> Svg msg
drawBrick now brick =
    let
        ( position, rotation ) =
            Brick.getBrickPosAndRot now brick

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
        , Svg.Attributes.fill <| colorToSvgString brick.fillColor
        , Svg.Attributes.stroke <| colorToSvgString Config.brickStrokeColor
        , Svg.Attributes.strokeWidth "1"
        ]
        []
