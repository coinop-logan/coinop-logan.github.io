module CommonView exposing (..)

import Element exposing (Attribute, Element)
import Html exposing (Html)
import Html.Attributes
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


stackElementsInZ2 : List (Element msg) -> Element msg
stackElementsInZ2 elements =
    let
        foldHelper : Element msg -> Element msg -> Element msg
        foldHelper newEl combinedPreviousEls =
            Element.el
                [ Element.inFront newEl ]
                combinedPreviousEls
    in
    List.foldl1
        foldHelper
        elements
        |> Maybe.withDefault Element.none


stackElementsInZ : List (Attribute msg) -> List (Element msg) -> Element msg
stackElementsInZ attrs elements =
    Element.html <|
        Html.div
            [ Html.Attributes.style "display" "grid" ]
            (elements
                |> List.indexedMap
                    (\i el ->
                        Html.div
                            [ Html.Attributes.style "grid-column" "1"
                            , Html.Attributes.style "grid-row" "1"
                            , Html.Attributes.style "z-index" (String.fromInt (i + 20))
                            ]
                            [ toHtml attrs el ]
                    )
            )
