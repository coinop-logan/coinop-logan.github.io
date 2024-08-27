module View exposing (..)

import BrickWall.Draw
import Browser
import Browser.Dom exposing (Viewport)
import CommonView exposing (..)
import Config
import Convert exposing (..)
import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Embed.Youtube
import Embed.Youtube.Attributes
import Fonts
import Responsive exposing (..)
import Route exposing (Route)
import TabGraphics
import Theme
import Time
import Types exposing (..)


root : Model -> Browser.Document Msg
root model =
    { title = "Logan Brutsche - Portfolio"
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
            Responsive.viewportToDisplayProfile model.viewport
    in
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Font.color Theme.defaultFontColor
        , Background.color Theme.bgColor
        ]
        [ headerEl dProfile model
        , bodyEl dProfile model
        ]


headerEl : DisplayProfile -> LoadedModel -> Element Msg
headerEl dProfile model =
    Element.el
        [ Element.width Element.fill
        , Element.height <| Element.px 134
        , Background.color Theme.headerBgColor
        ]
    <|
        Element.row
            [ Element.alignRight
            , Element.spacing 75
            , Element.centerY
            , Element.paddingXY 70 0
            , Font.size 20
            ]
            [ routingButton dProfile (Element.text "WORK") Route.Work model.route
            , routingButton dProfile (Element.text "ABOUT") Route.About model.route
            , routingButton dProfile (blueBorderedText dProfile "CONTACT") Route.Contact model.route
            ]


routingButton : DisplayProfile -> Element Msg -> Route -> Route -> Element Msg
routingButton dProfile labelEl route currentRoute =
    Input.button
        [ Element.padding 10 ]
        { label =
            Element.el
                (if route == currentRoute then
                    [ Font.bold ]

                 else
                    []
                )
                labelEl
        , onPress = Just <| GotoRoute route
        }


bodyEl : DisplayProfile -> LoadedModel -> Element Msg
bodyEl dProfile model =
    Element.el
        [ Element.clipY
        , Element.scrollbarX
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.behindContent <|
            case model.bodyViewport of
                Nothing ->
                    Element.none

                Just bodyViewport ->
                    BrickWall.Draw.view model.animateTime bodyViewport.scene.width bodyViewport.scene.height model.brickWall
        , addId "body-element"
        ]
    <|
        case model.route of
            Route.Work ->
                viewWorkPage dProfile

            Route.About ->
                viewAboutPage dProfile

            Route.Contact ->
                viewContactPage dProfile


viewWorkPage : DisplayProfile -> Element Msg
viewWorkPage dProfile =
    Element.text "viewWorkPage"


viewAboutPage : DisplayProfile -> Element Msg
viewAboutPage dProfile =
    Element.text "viewAboutPage"


viewContactPage : DisplayProfile -> Element Msg
viewContactPage dProfile =
    Element.text "viewContactPage"


blueBorderedText : DisplayProfile -> String -> Element Msg
blueBorderedText dProfile text =
    Element.el
        [ Element.padding 15
        , Font.color Theme.lightBlue
        , Border.rounded 100
        , Border.width 2
        , Border.color Theme.lightBlue
        ]
        (Element.text text)
