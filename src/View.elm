module View exposing (..)

import Browser
import CommonView exposing (..)
import Config
import Convert exposing (..)
import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Responsive exposing (DisplayProfile)
import TabGraphics
import Time
import Types exposing (..)


root : Model -> Browser.Document Msg
root model =
    { title = "Logan Brutsche"
    , body =
        [ Element.layoutWith
            { options =
                [ Element.focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
            }
            [ Element.width Element.fill
            , Element.height Element.fill

            -- , robotoFont
            ]
          <|
            case model of
                Loading _ ->
                    viewLoadingMessage

                Loaded loadedModel ->
                    view loadedModel
        ]
    }


viewLoadingMessage : Element Msg
viewLoadingMessage =
    Element.el
        [ Element.centerX
        , Element.padding 20
        ]
    <|
        Element.text "(loading)"


view : LoadedModel -> Element Msg
view model =
    let
        dProfile =
            model.dProfile
    in
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color Config.bgColor
        , Font.color Config.defaultFontColor
        ]
    <|
        Element.column
            [ Element.width Element.fill
            , Element.spacing 60
            , Element.padding 30
            ]
            [ headerElement dProfile
            , bodyElement dProfile model.tabState model.animateTime
            ]


headerElement : DisplayProfile -> Element Msg
headerElement dProfile =
    Element.column
        [ Element.centerX
        , Element.spacing 20
        ]
        [ nameElement dProfile
        , summaryElement dProfile
        ]


nameElement : DisplayProfile -> Element Msg
nameElement dProfile =
    Element.el
        [ Element.centerX
        , Font.size 80
        ]
    <|
        Element.text "Logan Brutsche"


summaryElement : DisplayProfile -> Element Msg
summaryElement dProfile =
    Element.el
        [ Element.centerX
        , Font.size 24
        ]
    <|
        Element.text "summary here summary here summary here summary here "


bodyElement : DisplayProfile -> TabState -> Time.Posix -> Element Msg
bodyElement dProfile tabState animateTime =
    let
        tabOnTop =
            case tabState of
                OnTab tab ->
                    tab

                SwitchingTo targetTab animateStartTime ->
                    let
                        progressFloat =
                            animationProgressFloat animateStartTime animateTime
                    in
                    if progressFloat < 0.5 then
                        otherTab targetTab

                    else
                        targetTab

        canvasWidth =
            1000

        tabBodyWidth =
            canvasWidth / 2

        xOffsetAbs =
            case tabState of
                OnTab _ ->
                    0

                SwitchingTo _ animateStartTime ->
                    let
                        easingFunction x =
                            if x == 1 then
                                1

                            else
                                1 - (2 ^ (-20 * x))

                        offsetMultiplier =
                            let
                                progressFloat =
                                    animationProgressFloat animateStartTime animateTime
                            in
                            if progressFloat < 0.5 then
                                easingFunction (progressFloat * 2)

                            else
                                1 - easingFunction ((progressFloat - 0.5) * 2)
                    in
                    offsetMultiplier * (tabBodyWidth / 2)

        portfolioTabEls =
            let
                tabTopWidth =
                    240
            in
            TabGraphics.createTabElementComponentsToStack
                { shapeBottomY = 400
                , bodyTopY = 100
                , tabTopY = 10
                , fillColor = Element.rgb 0 0 1
                , strokeColor = Element.rgb 1 0 0
                , pathThickness = 10
                , cornerRadius = 40
                , tabTopStartX = (canvasWidth / 2) - tabTopWidth
                , tabTopEndX = canvasWidth / 2
                , bodyExtendsLeft = ((tabBodyWidth / 2) - tabTopWidth) + xOffsetAbs
                , bodyExtendsRight = tabBodyWidth / 2 - xOffsetAbs
                , canvasWidth = Element.px <| canvasWidth
                }
                (Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                 <|
                    tabElement dProfile "Portfolio" PortfolioClicked
                )
                portfolioContentEl

        currentWorkTabEls =
            let
                tabTopWidth =
                    240
            in
            TabGraphics.createTabElementComponentsToStack
                { shapeBottomY = 400
                , bodyTopY = 100
                , tabTopY = 10
                , fillColor = Element.rgb 0 0 1
                , strokeColor = Element.rgb 1 0 0
                , pathThickness = 10
                , cornerRadius = 40
                , tabTopStartX = canvasWidth / 2
                , tabTopEndX = (canvasWidth / 2) + tabTopWidth
                , bodyExtendsLeft = tabBodyWidth / 2 - xOffsetAbs
                , bodyExtendsRight = ((tabBodyWidth / 2) - tabTopWidth) + xOffsetAbs
                , canvasWidth = Element.px <| canvasWidth
                }
                (Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                 <|
                    tabElement dProfile "Current Work" CurrentWorkClicked
                )
                currentWorkContentEl

        elsToStack =
            case tabOnTop of
                CurrentWork ->
                    [ portfolioTabEls.tabShape
                    , portfolioTabEls.bodyEl
                    , currentWorkTabEls.tabShape
                    , currentWorkTabEls.bodyEl
                    , portfolioTabEls.tabEl
                    , currentWorkTabEls.tabEl
                    ]

                Portfolio ->
                    [ currentWorkTabEls.tabShape
                    , currentWorkTabEls.bodyEl
                    , portfolioTabEls.tabShape
                    , portfolioTabEls.bodyEl
                    , currentWorkTabEls.tabEl
                    , portfolioTabEls.tabEl
                    ]
    in
    stackElementsInZ [ Element.centerX ] <| elsToStack


portfolioContentEl : Element Msg
portfolioContentEl =
    Element.text "portfoliooooo"


currentWorkContentEl : Element Msg
currentWorkContentEl =
    Element.text "current work waow"


tabElement : DisplayProfile -> String -> Msg -> Element Msg
tabElement dProfile label onPress =
    Input.button
        [ Font.size 36 ]
        { onPress = Just onPress
        , label = Element.text label
        }
