module CommonView exposing (..)

import Element exposing (Attribute, Element)
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import List.Extra as List


zIndex : Float -> Attribute msg
zIndex index =
    Element.htmlAttribute (Html.Attributes.style "z-index" (String.fromFloat index))


toHtml : List (Attribute msg) -> Element msg -> Html msg
toHtml attrs el =
    Element.layoutWith
        { options =
            [ Element.noStaticStyleSheet
            , Element.focusStyle
                { borderColor = Nothing
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        attrs
        el


stackElementsInZ : List (Attribute msg) -> List (Element msg) -> Element msg
stackElementsInZ attributes elements =
    let
        foldHelper : Element msg -> Element msg -> Element msg
        foldHelper newEl combinedPreviousEls =
            Element.el
                (attributes ++ [ Element.inFront newEl ])
                combinedPreviousEls
    in
    List.foldl1
        foldHelper
        elements
        |> Maybe.withDefault Element.none


fontMontserrat : Attribute msg
fontMontserrat =
    Font.family
        [ Font.typeface "montserrat"
        ]


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
