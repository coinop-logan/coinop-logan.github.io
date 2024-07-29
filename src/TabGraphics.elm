module TabGraphics exposing (..)

import Element exposing (Element)
import Point exposing (Point)
import Svg exposing (Svg)
import Svg.Attributes
import Types exposing (..)
import Utils


type alias TabSpec =
    { tabTopStartX : Float
    , tabTopEndX : Float
    , maybeBodyExtendsLeft : Maybe Float
    , maybeBodyExtendsRight : Maybe Float
    , shapeBottomY : Float
    , bodyTopY : Float
    , tabTopY : Float
    , fillColor : Element.Color
    , strokeColor : Element.Color
    , pathThickness : Float
    , cornerRadius : Float
    }


tabElement : TabSpec -> Element msg -> Element msg -> Element msg
tabElement tabSpec tabEl innerEl =
    let
        tabWidth =
            floor <|
                tabSpec.tabTopEndX
                    - tabSpec.tabTopStartX

        bodyWidth =
            tabWidth + (tabSpec.maybeBodyExtendsLeft |> Maybe.withDefault 0 |> floor) + (tabSpec.maybeBodyExtendsRight |> Maybe.withDefault 0 |> floor)

        innerElContained =
            Element.el
                [ Element.moveRight <| tabSpec.tabTopStartX - (tabSpec.maybeBodyExtendsLeft |> Maybe.withDefault 0)
                , Element.moveDown <| tabSpec.bodyTopY
                , Element.width <| Element.px bodyWidth
                ]
                innerEl

        tabElContained =
            Element.el
                [ Element.moveRight <| tabSpec.tabTopStartX
                , Element.moveDown <| tabSpec.tabTopY
                , Element.width <| Element.px tabWidth
                ]
                tabEl
    in
    Element.el
        [ Element.width <| Element.px 600
        , Element.height <| Element.px 600
        , Element.centerX
        , Element.inFront tabElContained
        , Element.inFront innerElContained
        ]
    <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.height "100%" ]
                [ Svg.defs
                    []
                    []
                , drawTabShape
                    tabSpec
                ]


tabShapeSpecTest : TabSpec
tabShapeSpecTest =
    { tabTopStartX = 100
    , tabTopEndX = 300
    , maybeBodyExtendsLeft = Just 20
    , maybeBodyExtendsRight = Just 15
    , shapeBottomY = 500
    , bodyTopY = 100
    , tabTopY = 20
    , fillColor = Element.rgb 0 0 1
    , strokeColor = Element.rgb 1 0 0
    , pathThickness = 3
    , cornerRadius = 20
    }


drawTabShape : TabSpec -> Svg msg
drawTabShape tabSpec =
    let
        shapeStartX =
            tabSpec.tabTopStartX - (tabSpec.maybeBodyExtendsLeft |> Maybe.withDefault 0)

        shapeEndX =
            tabSpec.tabTopEndX + (tabSpec.maybeBodyExtendsRight |> Maybe.withDefault 0)

        shapeStartPoint =
            { x = shapeStartX, y = tabSpec.shapeBottomY }

        dString =
            String.join " "
                [ moveToPointString shapeStartPoint
                , drawToPointString { x = shapeStartPoint.x, y = tabSpec.bodyTopY + tabSpec.cornerRadius }
                , case tabSpec.maybeBodyExtendsLeft of
                    Nothing ->
                        drawToPointString { x = shapeStartPoint.x, y = tabSpec.bodyTopY - tabSpec.cornerRadius }

                    Just extendsLeftAmount ->
                        if extendsLeftAmount > tabSpec.cornerRadius * 2 then
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
                , case tabSpec.maybeBodyExtendsRight of
                    Nothing ->
                        drawToPointString { x = shapeEndX, y = tabSpec.bodyTopY + tabSpec.cornerRadius }

                    Just extendsRightAmount ->
                        if extendsRightAmount > tabSpec.cornerRadius * 2 then
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
