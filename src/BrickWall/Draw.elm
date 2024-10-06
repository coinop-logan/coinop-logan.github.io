module BrickWall.Draw exposing (..)

import BrickWall.Brick as Brick exposing (Brick)
import BrickWall.BrickWall as BrickWall exposing (BrickWall)
import BrickWall.BricksContainer as BricksContainer
import BrickWall.Common exposing (..)
import BrickWall.Config as Config
import Browser.Dom exposing (Viewport)
import Element exposing (Element)
import List.Extra as List
import Maybe.Extra as Maybe
import Point exposing (Point)
import Responsive exposing (DisplayProfile)
import Svg exposing (Svg)
import Svg.Attributes
import SvgHelpers exposing (colorToSvgString, drawToPointString, moveToPointString, pointToString)
import Time


view : Time.Posix -> BrickWall -> Element msg
view now brickWall =
    Element.el [ Element.clipX, Element.width Element.fill ] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.width <| String.fromFloat brickWall.bodyViewport.scene.width
                , Svg.Attributes.height <| String.fromFloat brickWall.bodyViewport.scene.height
                ]
                [ Svg.defs
                    []
                    (radialGradientDefs brickWall)
                , draw now brickWall
                ]


radialGradientDefs : BrickWall -> List (Svg msg)
radialGradientDefs brickWall =
    let
        gradientCircleCenters =
            let
                numPointsNeeded =
                    (heightToFillWithCircleGradientPoints brickWall.bodyViewport / vSpaceBetweenCircleGradientPoints brickWall.bodyViewport) + 1 |> floor
            in
            -- start with a list of alternating values we will use for the x value of the point
            List.cycle numPointsNeeded
                [ brickWall.bodyViewport.viewport.width, 0.0 ]
                |> List.indexedMap
                    (\i x ->
                        { x = x
                        , y = toFloat i * vSpaceBetweenCircleGradientPoints brickWall.bodyViewport
                        }
                    )

        maxGridposInfluenced =
            { x = brickWall.bodyViewport.viewport.width + circleGradientRadius brickWall.bodyViewport
            , y = heightToFillWithCircleGradientPoints brickWall.bodyViewport + circleGradientRadius brickWall.bodyViewport
            }
                |> realPosToGridPos (calcBrickDims brickWall.bodyViewport)
    in
    List.range 0 (BricksContainer.gridPosToListPos brickWall.bricks maxGridposInfluenced)
        |> List.map (BricksContainer.listPosToGridPos brickWall.bricks.numColumns)
        |> List.map
            (\gridPos ->
                let
                    realPos =
                        gridPosToRealPos (calcBrickDims brickWall.bodyViewport) gridPos

                    closestGradientPoint =
                        gradientCircleCenters
                            |> List.minimumBy (Point.manhattanDistance realPos)
                            |> Maybe.withDefault { x = 0, y = 0 }

                    localPoint =
                        Point.sub closestGradientPoint realPos
                in
                Svg.radialGradient
                    [ Svg.Attributes.id <| gradientIdStr gridPos
                    , Svg.Attributes.cx <| String.fromFloat (localPoint.x / toFloat (calcBrickWidth brickWall.bodyViewport))
                    , Svg.Attributes.cy <| String.fromFloat (localPoint.y / toFloat (calcBrickWidth brickWall.bodyViewport))
                    , Svg.Attributes.r <| String.fromFloat (circleGradientRadius brickWall.bodyViewport / toFloat (calcBrickWidth brickWall.bodyViewport))
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
        fadedAreas =
            -- case brickWall.titleArea of
            --     Just area ->
            --         [ padArea 55 30 area ]
            --     Nothing ->
            []

        drawnBricks =
            brickWall.bricks
                |> BricksContainer.toList
                |> List.map (Maybe.map (drawBrick brickWall.bodyViewport now fadedAreas))
                |> Maybe.values
    in
    Svg.g [] drawnBricks


drawBrick : Viewport -> Time.Posix -> List AreaDef -> Brick -> Svg msg
drawBrick bodyViewport now fadedAreas brick =
    let
        isFaded =
            List.any
                (\area ->
                    pointIsInArea (pointToCenterPoint bodyViewport brick.homePoint) area
                )
                fadedAreas

        ( position, rotation ) =
            Brick.getBrickPosAndRot now brick

        transformString =
            String.join " "
                [ SvgHelpers.rotateString rotation
                ]

        rectAttrs =
            [ Svg.Attributes.x <| String.fromInt <| floor position.x
            , Svg.Attributes.y <| String.fromInt <| floor position.y
            , Svg.Attributes.class "brickrect"
            , Svg.Attributes.width <| String.fromInt <| calcBrickWidth bodyViewport
            , Svg.Attributes.height <| String.fromInt <| calcBrickHeight bodyViewport
            , Svg.Attributes.transform transformString
            , Svg.Attributes.strokeWidth "1"
            ]
                ++ (if isFaded then
                        [ Svg.Attributes.fill <| "url(#" ++ brick.gradientUrl ++ ")"
                        , Svg.Attributes.stroke "white"
                        , Svg.Attributes.strokeOpacity <| String.fromFloat Config.fadedBrickStrokeOpacity
                        , Svg.Attributes.fillOpacity <| String.fromFloat Config.fadedBrickFillOpacity
                        ]

                    else
                        [ Svg.Attributes.fill <| "url(#" ++ brick.gradientUrl ++ ")"
                        , Svg.Attributes.stroke "white"
                        , Svg.Attributes.strokeOpacity <| String.fromFloat Config.brickStrokeOpacity
                        ]
                   )
    in
    Svg.rect
        rectAttrs
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


heightToFillWithCircleGradientPoints : Viewport -> Float
heightToFillWithCircleGradientPoints bodyViewport =
    bodyViewport.scene.height


vSpaceBetweenCircleGradientPoints : Viewport -> Float
vSpaceBetweenCircleGradientPoints bodyViewport =
    bodyViewport.scene.width


circleGradientRadius : Viewport -> Float
circleGradientRadius bodyViewport =
    bodyViewport.scene.width / 2
