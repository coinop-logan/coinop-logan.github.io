module View exposing (..)

import Browser
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

        xOffsetAbs =
            let
                easingFunction x =
                    if x == 1 then
                        1

                    else
                        1 - (2 ^ (-20 * x))

                offsetMultiplier =
                    case tabState of
                        OnTab _ ->
                            0

                        SwitchingTo _ animateStartTime ->
                            let
                                progressFloat =
                                    animationProgressFloat animateStartTime animateTime
                            in
                            if progressFloat < 0.5 then
                                easingFunction (progressFloat * 2)

                            else
                                1 - easingFunction ((progressFloat - 0.5) * 2)
            in
            offsetMultiplier * (100.0 / 2)
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing 20
        ]
        [ tabsElement dProfile tabState

        -- , tabBody dProfile tabState
        , TabGraphics.tabElement
            { tabTopStartX = 100
            , tabTopEndX = 300
            , maybeBodyExtendsLeft = Just xOffsetAbs
            , maybeBodyExtendsRight = Just <| 100 - xOffsetAbs
            , shapeBottomY = 500
            , bodyTopY = 100
            , tabTopY = 20
            , fillColor = Element.rgb 0 0 1
            , strokeColor = Element.rgb 1 0 0
            , pathThickness = 3
            , cornerRadius = 20
            }
            (Element.el [ Element.centerX, Element.padding 10 ] <| Element.text "tab")
            (Element.column [ Element.spacing 4 ]
                [ Element.text "hi"
                , Element.text "hi"
                , Element.text "hi"
                , Element.text "hi"
                ]
            )
        ]


tabsElement : DisplayProfile -> TabState -> Element Msg
tabsElement dProfile tabState =
    Element.row
        [ Element.centerX
        , Element.spacing 50
        , Font.size 30
        ]
        [ tabElement dProfile "Current Work" CurrentWorkClicked
        , tabElement dProfile "Portfolio" PortfolioClicked
        ]


tabElement : DisplayProfile -> String -> Msg -> Element Msg
tabElement dProfile label onPress =
    Input.button
        [ Font.size 36 ]
        { onPress = Just onPress
        , label = Element.text label
        }



-- tabBody : DisplayProfile -> TabState -> Element Msg
-- tabBody dProfile tabState =
--     case tabState of
--         OnTab tab ->
--             case tab of
--                 CurrentWork ->
--                     currentWorkBody dProfile
--                 Portfolio ->
--                     portfolioBody dProfile


currentWorkBody : DisplayProfile -> Element Msg
currentWorkBody dProfile =
    Element.el [ Element.centerX ] <|
        Element.text "currentWorkBody"


portfolioBody : DisplayProfile -> Element Msg
portfolioBody dProfile =
    Element.el [ Element.centerX ] <|
        Element.text "portfolioBody"
