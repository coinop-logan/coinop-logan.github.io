module BrickWall.Draw exposing (..)

import BrickWall.Brick as Brick exposing (Brick)
import BrickWall.BrickWall as BrickWall exposing (BrickWall)
import BrickWall.BricksContainer as BricksContainer
import BrickWall.Common exposing (..)
import BrickWall.Config as Config
import Element exposing (Element)
import List.Extra as List
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
                    (radialGradientDefs width)
                , draw now model
                ]


radialGradientDefs : Float -> List (Svg msg)
radialGradientDefs screenWidth =
    let
        gradientCircleCenters =
            let
                numPointsNeeded =
                    (Config.heightToFillWithCircleGradientPoints / Config.vSpaceBetweenCircleGradientPoints) + 1 |> floor
            in
            -- start with a list of alternating values we will use for the x value of the point
            List.cycle numPointsNeeded
                [ screenWidth, 0.0 ]
                |> List.indexedMap
                    (\i x ->
                        { x = x
                        , y = toFloat i * Config.vSpaceBetweenCircleGradientPoints
                        }
                    )

        maxGridposInfluenced =
            { x = screenWidth + Config.circleGradientRadius
            , y = Config.heightToFillWithCircleGradientPoints + Config.circleGradientRadius
            }
                |> realPosToGridPos
    in
    List.range 0 (BricksContainer.gridPosToListPos maxGridposInfluenced)
        |> List.map BricksContainer.listPosToGridPos
        |> List.map
            (\gridPos ->
                let
                    realPos =
                        gridPosToRealPos gridPos

                    closestGradientPoint =
                        gradientCircleCenters
                            |> List.minimumBy (Point.manhattanDistance realPos)
                            |> Maybe.withDefault { x = 0, y = 0 }

                    localPoint =
                        Point.sub closestGradientPoint realPos
                in
                Svg.radialGradient
                    [ Svg.Attributes.id <| gradientIdStr gridPos
                    , Svg.Attributes.cx <| String.fromFloat (localPoint.x / Config.brickWidth)
                    , Svg.Attributes.cy <| String.fromFloat (localPoint.y / Config.brickWidth)
                    , Svg.Attributes.r <| String.fromFloat (Config.circleGradientRadius / Config.brickWidth)
                    ]
                    [ Svg.stop
                        [ Svg.Attributes.offset "0%"
                        , Svg.Attributes.stopColor <| colorToSvgString Config.brickGradientColor
                        ]
                        []
                    , Svg.stop
                        [ Svg.Attributes.offset "100%"
                        , Svg.Attributes.stopColor <| colorToSvgString Config.brickMainColor
                        ]
                        []
                    ]
            )


draw : Time.Posix -> BrickWall -> Svg msg
draw now brickWall =
    let
        drawnBricks =
            brickWall.bricks
                |> BricksContainer.toList
                |> List.map (Maybe.andThen (maybeDrawBrick now brickWall.titleArea))
                |> Maybe.values
    in
    Svg.g [] drawnBricks


maybeDrawBrick : Time.Posix -> Maybe AreaDef -> Brick -> Maybe (Svg msg)
maybeDrawBrick now maybeTitleArea brick =
    case maybeTitleArea of
        Nothing ->
            Just <| drawBrick now brick

        Just nameArea ->
            if pointIsInArea (pointToCenterPoint brick.homePoint) (padArea 60 30 nameArea) then
                Just <|
                    drawBrick now brick
                -- { brick
                --     | fillColor = Element.rgb 0.1 0 0
                --     , strokeColor = Config.brickBehindNameStrokeColor
                -- }

            else
                Just <| drawBrick now brick


drawBrick : Time.Posix -> Brick -> Svg msg
drawBrick now brick =
    let
        testGradPointGlobal =
            { x = 100, y = 500 }

        gradPointLocal =
            Point.sub testGradPointGlobal brick.homePoint

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
        , Svg.Attributes.fill <| "url(#" ++ brick.gradientUrl ++ ")"
        , Svg.Attributes.stroke "white"
        , Svg.Attributes.strokeOpacity "0.05"
        , Svg.Attributes.strokeWidth "1"
        ]
        []


odrawBrick : Time.Posix -> Brick -> Svg msg
odrawBrick now brick =
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

        -- , Svg.Attributes.fill <| colorToSvgString brick.fillColor
        -- , Svg.Attributes.stroke <| colorToSvgString brick.strokeColor
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
