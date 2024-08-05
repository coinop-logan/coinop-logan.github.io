module TabGraphics exposing (..)

import CommonView
import Element exposing (Attribute, Element)
import Element.Background as Background
import Html.Attributes
import Point exposing (Point)
import Svg exposing (Svg)
import Svg.Attributes
import Types exposing (..)
import Utils


type alias TabSpec =
    { tabTopStartX : Float
    , tabTopEndX : Float
    , bodyExtendsLeft : Float
    , bodyExtendsRight : Float
    , shapeBottomY : Float
    , bodyTopY : Float
    , tabTopY : Float
    , fillColor : Element.Color
    , strokeColor : Element.Color
    , pathThickness : Float
    , cornerRadius : Float
    , canvasWidth : Element.Length
    }


createTabElementComponentsToStack : TabSpec -> Element msg -> Element msg -> { tabShape : Element msg, tabEl : Element msg, bodyEl : Element msg }
createTabElementComponentsToStack tabSpec tabEl innerEl =
    let
        tabWidth =
            floor <|
                tabSpec.tabTopEndX
                    - tabSpec.tabTopStartX

        bodyWidth =
            tabWidth + (tabSpec.bodyExtendsLeft |> floor) + (tabSpec.bodyExtendsRight |> floor)

        innerElContained =
            Element.el
                [ Element.moveRight <| tabSpec.tabTopStartX - tabSpec.bodyExtendsLeft
                , Element.moveDown <| tabSpec.bodyTopY
                , Element.width <| Element.px bodyWidth
                , Element.height <| Element.px <| floor <| tabSpec.shapeBottomY - tabSpec.bodyTopY
                ]
                innerEl

        tabElContained =
            Element.el
                [ Element.moveRight <| tabSpec.tabTopStartX
                , Element.moveDown <| tabSpec.tabTopY
                , Element.width <| Element.px tabWidth
                , Element.height <| Element.px <| floor <| tabSpec.bodyTopY - tabSpec.tabTopY
                ]
                tabEl
    in
    { tabShape =
        Element.el [ Element.width tabSpec.canvasWidth, Element.height Element.fill ] <|
            Element.html <|
                Svg.svg
                    [ Svg.Attributes.height <| String.fromFloat <| tabSpec.shapeBottomY + (tabSpec.pathThickness / 2)
                    ]
                    [ Svg.defs
                        []
                        []
                    , drawTabShape tabSpec
                    ]
    , tabEl = tabElContained
    , bodyEl = innerElContained
    }


drawTabShape : TabSpec -> Svg msg
drawTabShape tabSpec =
    let
        shapeStartX =
            tabSpec.tabTopStartX - tabSpec.bodyExtendsLeft

        shapeEndX =
            tabSpec.tabTopEndX + tabSpec.bodyExtendsRight

        shapeStartPoint =
            { x = shapeStartX, y = tabSpec.shapeBottomY }

        dString =
            String.join " "
                [ moveToPointString shapeStartPoint
                , drawToPointString { x = shapeStartPoint.x, y = tabSpec.bodyTopY + tabSpec.cornerRadius }
                , if tabSpec.bodyExtendsLeft == 0 then
                    drawToPointString { x = shapeStartPoint.x, y = tabSpec.bodyTopY - tabSpec.cornerRadius }

                  else if tabSpec.bodyExtendsLeft > tabSpec.cornerRadius * 2 then
                    String.join " "
                        [ drawThroughElbowString True { x = shapeStartX + tabSpec.cornerRadius, y = tabSpec.bodyTopY } tabSpec.cornerRadius
                        , drawToPointString { x = tabSpec.tabTopStartX - tabSpec.cornerRadius, y = tabSpec.bodyTopY }
                        , drawThroughElbowString False { x = tabSpec.tabTopStartX, y = tabSpec.bodyTopY - tabSpec.cornerRadius } tabSpec.cornerRadius
                        ]

                  else
                    let
                        startPoint =
                            { x = shapeStartPoint.x, y = tabSpec.bodyTopY + tabSpec.cornerRadius }

                        endPoint =
                            { x = tabSpec.tabTopStartX, y = tabSpec.bodyTopY - tabSpec.cornerRadius }

                        midPoint =
                            Point.getMidpoint startPoint endPoint
                    in
                    String.join " "
                        [ drawThroughArcWithVerticalEnd startPoint midPoint True
                        , drawThroughArcWithVerticalEnd midPoint endPoint False
                        ]
                , drawToPointString { x = tabSpec.tabTopStartX, y = tabSpec.tabTopY + tabSpec.cornerRadius }
                , drawThroughElbowString True { x = tabSpec.tabTopStartX + tabSpec.cornerRadius, y = tabSpec.tabTopY } tabSpec.cornerRadius
                , drawToPointString { x = tabSpec.tabTopEndX - tabSpec.cornerRadius, y = tabSpec.tabTopY }
                , drawThroughElbowString True { x = tabSpec.tabTopEndX, y = tabSpec.tabTopY + tabSpec.cornerRadius } tabSpec.cornerRadius
                , drawToPointString { x = tabSpec.tabTopEndX, y = tabSpec.bodyTopY - tabSpec.cornerRadius }
                , if tabSpec.bodyExtendsRight == 0 then
                    drawToPointString { x = shapeEndX, y = tabSpec.bodyTopY + tabSpec.cornerRadius }

                  else if tabSpec.bodyExtendsRight > tabSpec.cornerRadius * 2 then
                    String.join " "
                        [ drawThroughElbowString False { x = tabSpec.tabTopEndX + tabSpec.cornerRadius, y = tabSpec.bodyTopY } tabSpec.cornerRadius
                        , drawToPointString { x = shapeEndX - tabSpec.cornerRadius, y = tabSpec.bodyTopY }
                        , drawThroughElbowString True { x = shapeEndX, y = tabSpec.bodyTopY + tabSpec.cornerRadius } tabSpec.cornerRadius
                        ]

                  else
                    let
                        startPoint =
                            { x = tabSpec.tabTopEndX, y = tabSpec.bodyTopY - tabSpec.cornerRadius }

                        endPoint =
                            { x = shapeEndX, y = tabSpec.bodyTopY + tabSpec.cornerRadius }

                        midPoint =
                            Point.getMidpoint startPoint endPoint
                    in
                    String.join " "
                        [ drawThroughArcWithVerticalEnd startPoint midPoint False
                        , drawThroughArcWithVerticalEnd midPoint endPoint True
                        ]
                , drawToPointString { x = shapeEndX, y = tabSpec.shapeBottomY }
                , "Z"
                ]
    in
    Svg.path
        [ Svg.Attributes.d dString
        , Svg.Attributes.fill <| colorToSvgString tabSpec.fillColor
        , Svg.Attributes.stroke <| colorToSvgString tabSpec.strokeColor
        , Svg.Attributes.strokeWidth <| String.fromFloat tabSpec.pathThickness
        ]
        []


drawThroughArcWithVerticalEnd : Point -> Point -> Bool -> String
drawThroughArcWithVerticalEnd startPoint endPoint clockwise =
    let
        y =
            abs <|
                endPoint.y
                    - startPoint.y

        x =
            abs <|
                endPoint.x
                    - startPoint.x

        arcRadius =
            y / cos (atan (y / x) - atan (x / y))
    in
    drawThroughElbowString clockwise endPoint arcRadius


drawThroughElbowString : Bool -> Point -> Float -> String
drawThroughElbowString clockwise toPoint r =
    [ "A"
    , String.fromFloat r
    , String.fromFloat r
    , String.fromInt 0 -- rotation
    , String.fromInt 0
    , String.fromInt <| boolToInt clockwise
    , String.fromFloat toPoint.x
    , String.fromFloat toPoint.y
    ]
        |> String.join " "


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
