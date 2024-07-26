module View exposing (..)

import Browser
import Config
import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Font as Font
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
            , bodyElement dProfile
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


bodyElement : DisplayProfile -> Element Msg
bodyElement dProfile =
    Element.text "body"
