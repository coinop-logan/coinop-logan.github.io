module View exposing (..)

import Browser
import Config
import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Responsive exposing (DisplayProfile)
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
            , Element.spacing 15
            , Element.padding 30
            ]
            [ headerElement dProfile
            , bodyElement dProfile model.tabState
            ]


headerElement : DisplayProfile -> Element Msg
headerElement dProfile =
    Element.column
        [ Element.centerX
        , Element.spacing 10
        ]
        [ nameElement dProfile
        , summaryElement dProfile
        ]


nameElement : DisplayProfile -> Element Msg
nameElement dProfile =
    Element.el
        [ Element.centerX
        , Font.size 50
        ]
    <|
        Element.text "Logan Brutsche"


summaryElement : DisplayProfile -> Element Msg
summaryElement dProfile =
    Element.el
        [ Element.centerX
        , Font.size 18
        ]
    <|
        Element.text "summary here summary here summary here summary here "


bodyElement : DisplayProfile -> TabState -> Element Msg
bodyElement dProfile tabState =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 20
        ]
        [ tabsElement dProfile tabState
        , tabBody dProfile tabState
        ]


tabsElement : DisplayProfile -> TabState -> Element Msg
tabsElement dProfile tabState =
    Element.row
        [ Element.centerX
        , Element.spacing 30
        , Font.size 30
        ]
        [ tabElement dProfile "Current Work" CurrentWorkClicked
        , tabElement dProfile "Portfolio" PortfolioClicked
        ]


tabElement : DisplayProfile -> String -> Msg -> Element Msg
tabElement dProfile label onPress =
    Input.button
        []
        { onPress = Just onPress
        , label = Element.text label
        }


tabBody : DisplayProfile -> TabState -> Element Msg
tabBody dProfile tabState =
    case tabState of
        CurrentWork ->
            currentWorkBody dProfile

        Portfolio ->
            portfolioBody dProfile


currentWorkBody : DisplayProfile -> Element Msg
currentWorkBody dProfile =
    Element.el [ Element.centerX ] <|
        Element.text "currentWorkBody"


portfolioBody : DisplayProfile -> Element Msg
portfolioBody dProfile =
    Element.el [ Element.centerX ] <|
        Element.text "portfolioBody"
