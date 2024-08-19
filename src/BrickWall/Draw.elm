module BrickWall.Draw exposing (..)

import BrickWall.Brick as Brick exposing (Brick)
import BrickWall.BrickWall as BrickWall exposing (BrickWall)
import BrickWall.BricksContainer as BricksContainer
import BrickWall.Common exposing (..)
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
    Element.el [ Element.clipX, Element.width Element.fill ] <|
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
                |> List.map (Maybe.andThen (maybeDrawBrick now brickWall.nameArea))
                |> Maybe.values
    in
    Svg.g [] drawnBricks


maybeDrawBrick : Time.Posix -> Maybe AreaDef -> Brick -> Maybe (Svg msg)
maybeDrawBrick now maybeNameArea brick =
    case maybeNameArea of
        Nothing ->
            Just <| drawBrick now brick

        Just nameArea ->
            if pointIsInArea (pointToCenterPoint brick.homePoint) (padArea 60 40 nameArea) then
                Just <|
                    drawBrick now
                        { brick
                            | fillColor = Element.rgb 0.1 0 0
                            , strokeColor = Config.brickBehindNameStrokeColor
                        }

            else
                Just <| drawBrick now brick


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
        [ Svg.Attributes.x <| String.fromInt <| floor position.x
        , Svg.Attributes.y <| String.fromInt <| floor position.y
        , Svg.Attributes.class "brickrect"
        , Svg.Attributes.width <| String.fromInt Config.brickWidth
        , Svg.Attributes.height <| String.fromInt Config.brickHeight
        , Svg.Attributes.transform transformString
        , Svg.Attributes.fill <| colorToSvgString brick.fillColor
        , Svg.Attributes.stroke <| colorToSvgString brick.strokeColor
        , Svg.Attributes.strokeWidth "1"
        ]
        []


pointIsInArea : Point -> AreaDef -> Bool
pointIsInArea point area =
    point.x
        > area.x
        && point.x
        < area.x
        + area.width
        && point.y
        > area.y
        && point.y
        < area.y
        + area.height
