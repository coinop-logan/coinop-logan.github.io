module CommonView exposing (..)

import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes


zIndex : Float -> Attribute msg
zIndex index =
    Element.htmlAttribute (Html.Attributes.style "z-index" (String.fromFloat index))


linkAttributes : List (Attribute msg)
linkAttributes =
    [ Font.color <| Element.rgb 0.8 0.8 1
    , Font.underline
    ]


newTabLink : List (Attribute msg) -> String -> String -> Element msg
newTabLink extraAttributes url labelText =
    Element.newTabLink
        (linkAttributes ++ extraAttributes)
        { url = url
        , label = Element.text labelText
        }


addId : String -> Attribute msg
addId idStr =
    Element.htmlAttribute <|
        Html.Attributes.id idStr


hbreak : Int -> Element.Color -> Element msg
hbreak height color =
    Element.el
        [ Element.width Element.fill
        , Element.height <| Element.px height
        , Background.color color
        ]
        Element.none
