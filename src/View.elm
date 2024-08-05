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
import Theme
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
        , Font.color Theme.defaultFontColor
        ]
    <|
        Element.column
            [ Element.width Element.fill
            , Background.color Theme.bgColor
            , Element.height Element.fill
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
        , Font.semiBold
        , fontMontserrat
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
            1600

        tabBodyWidth =
            canvasWidth / 2

        tabSeparation =
            10

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
                { shapeBottomY = 1500
                , bodyTopY = 100
                , tabTopY = 10
                , fillColor = Theme.portfolioTabBackgroundColor
                , strokeColor = Theme.tabBorderColor
                , pathThickness = 6
                , cornerRadius = 40
                , tabTopStartX = (canvasWidth / 2) - tabTopWidth - (tabSeparation / 2)
                , tabTopEndX = canvasWidth / 2 - (tabSeparation / 2)
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
                (portfolioContentEl dProfile)

        currentWorkTabEls =
            let
                tabTopWidth =
                    240
            in
            TabGraphics.createTabElementComponentsToStack
                { shapeBottomY = 1500
                , bodyTopY = 100
                , tabTopY = 10
                , fillColor = Theme.currentWorkTabBackgroundColor
                , strokeColor = Theme.tabBorderColor
                , pathThickness = 6
                , cornerRadius = 40
                , tabTopStartX = canvasWidth / 2 + (tabSeparation / 2)
                , tabTopEndX = (canvasWidth / 2) + tabTopWidth + (tabSeparation / 2)
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
    stackElementsInZ [ Element.centerX, Element.height Element.fill ] <| elsToStack


portfolioContentEl : DisplayProfile -> Element Msg
portfolioContentEl dProfile =
    Element.column
        [ Element.width Element.fill
        , Element.padding 45
        , Element.spacing 35
        ]
        [ portfolioEntryEl dProfile
            (Element.image
                [ Element.height <| Element.px 60 ]
                { src = "/public/coinfight-title.png"
                , description = "coinfight"
                }
            )
            "2022 / 2023"
            [ "An RTS game where users fight over crypto in-game. Players must invest real crypto into their armies, which if killed is dropped onto the battlefield for anyone else to pick up, capture, and withdraw. This is a zero-sum game where the goal is to get more out than you put in. \"Like Poker, but the chips shoot at each other!\""
            , "The goal of Coinfight was to give players the tangible experience of fighting in a virtual match over real money in real time. This was achieved without invoking the usual cumbersome blockchain constraints by treating the blockchain more as a clearing house than as a place for game state, contrary to the usual approach for web3 gaming."
            ]
            [ newTabLink [] "https://www.youtube.com/watch?v=7tw10KUO1_U" "demo video"
            , newTabLink [] "https://medium.com/p/472636deec57" "dev blog post"
            , newTabLink [] "https://coinfight.io/" "coinfight.io"
            ]
        ]


currentWorkContentEl : Element Msg
currentWorkContentEl =
    Element.column [ Element.spacing 5 ]
        (List.repeat 10 <| Element.text "current work waoww")


portfolioEntryEl : DisplayProfile -> Element Msg -> String -> List String -> List (Element Msg) -> Element Msg
portfolioEntryEl dProfile titleEl dateString bodyStrings linkOutEls =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 20
        ]
        [ Element.row
            [ Element.width Element.fill ]
            [ titleEl
            , Element.el
                [ Element.alignRight
                , Element.centerY
                , Font.size 28
                ]
              <|
                Element.text dateString
            ]
        , Element.column
            [ Element.width Element.fill
            , Element.spacing 15
            , Font.size 18
            ]
            (bodyStrings
                |> List.map
                    (\bodyString ->
                        Element.paragraph
                            [ Element.paddingXY 30 0 ]
                            [ Element.text bodyString ]
                    )
            )
        , Element.row
            [ Element.spacing 30 ]
            linkOutEls
        ]


tabElement : DisplayProfile -> String -> Msg -> Element Msg
tabElement dProfile label onPress =
    Input.button
        [ Font.size 36 ]
        { onPress = Just onPress
        , label = Element.text label
        }
