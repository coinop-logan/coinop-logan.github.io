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
import Html
import Html.Attributes
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
            , Fonts.poppins
            , Font.bold
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
                    [ Font.extraBold
                    ]

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


viewAboutPage : DisplayProfile -> Element Msg
viewAboutPage dProfile =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 140
        ]
        [ nameAndTitleElement dProfile
        , viewPortfolioElements dProfile
        ]


nameAndTitleElement : DisplayProfile -> Element Msg
nameAndTitleElement dProfile =
    Element.column
        [ Element.centerX
        , Element.spacing 44
        , Fonts.poppins
        , Element.paddingEach
            { top = 135
            , bottom = 0
            , right = 0
            , left = 0
            }
        ]
        [ Element.el
            [ Element.centerX
            , Font.size 80
            , Font.extraBold
            ]
          <|
            nameElement
        , Element.el
            [ Element.centerX
            , Font.size 40
            ]
          <|
            Element.text "Full-Stack Software Architect"
        ]


nameElement : Element Msg
nameElement =
    Element.html <|
        Html.div
            [ Html.Attributes.style "background" "-webkit-linear-gradient(left, white, #009DC5)"
            , Html.Attributes.style "-webkit-background-clip" "text"
            , Html.Attributes.style "-webkit-text-fill-color" "transparent"
            , Html.Attributes.style "line-height" "normal"
            ]
        <|
            [ Html.text "Logan Brutsche" ]


nameGradientStyles : List (Attribute Msg)
nameGradientStyles =
    List.map Element.htmlAttribute
        [ Html.Attributes.style "background" "#eee"
        , Html.Attributes.style "background" "-webkit-linear-gradient(left, #eee, #333)"
        , Html.Attributes.style "-webkit-background-clip" "text"
        , Html.Attributes.style "-webkit-text-fill-color" "transparent"
        ]


viewPortfolioElements : DisplayProfile -> Element Msg
viewPortfolioElements dProfile =
    Element.column
        [ Element.centerX
        , Element.spacing 132
        , Element.width Element.fill
        ]
        [ portfolioEntryEl dProfile
            (Element.image
                [ Element.height <| Element.px 50 ]
                { src = "eestisse-title.png"
                , description = "eestisse"
                }
            )
            "2024"
            "Solo Project"
            [ "An LLM-powered tool that explains the counter-intuitive Estonian grammar to English speakers (for example, why \"eestisse\" means \"into Estonia\"). The tool takes English or Estonian text, translates it, and explains word by word how the Estonian is constructed."
            , "The central feature was surprisingly easy to build, due to LLM's strength in language tasks."
            ]
            [ newTabLink [] "https://eestisse.ee" "eestisse.ee"
            , newTabLink [] "https://github.com/eestisse/eestisse" "github"
            ]
        ]


viewWorkPage : DisplayProfile -> Element Msg
viewWorkPage dProfile =
    Element.text "viewWorkPage"


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


portfolioEntryEl : DisplayProfile -> Element Msg -> String -> String -> List String -> List (Element Msg) -> Element Msg
portfolioEntryEl dProfile titleEl dateString roleString bodyStrings linkOutEls =
    Element.column
        [ Background.color <| Element.rgba255 217 217 217 0.2
        , Border.rounded 40
        , Element.paddingXY 60 43
        , Element.width (Element.fill |> Element.maximum 1050)
        , Element.centerX
        ]
        [ Element.text "stuff's gonna go here :D"
        ]
