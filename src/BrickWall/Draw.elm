module BrickWall.Draw exposing (..)

import BrickWall.BrickWall as BrickWall exposing (BrickWall)
import BrickWall.Config as Config
import BrickWall.State as State
import BrickWall.Types exposing (..)
import Element exposing (Element)
import Maybe.Extra as Maybe
import Point exposing (Point)
import Svg exposing (Svg)
import Svg.Attributes
import SvgHelpers exposing (colorToSvgString, drawToPointString, moveToPointString, pointToString)
import Time


view : Time.Posix -> BrickWall -> Element msg
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


draw : Time.Posix -> BrickWall -> Svg msg
draw now model =
    let
        drawnBricks =
            model.bricks
                |> BrickWall.toList
                |> List.map (Maybe.map (drawBrick now))
                |> Maybe.values
    in
    Svg.g [] drawnBricks


drawBrick : Time.Posix -> Brick -> Svg msg
drawBrick now brick =
    let
        ( position, rotation ) =
            State.getBrickPosAndRot now brick

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
