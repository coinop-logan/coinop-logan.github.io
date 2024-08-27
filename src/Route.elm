module Route exposing (..)

import Url exposing (Url)
import Url.Builder as UB
import Url.Parser as UP


type Route
    = Work
    | About
    | Contact


parseUrl : Url -> Route
parseUrl url =
    UP.parse routeParser url
        |> Maybe.withDefault About


routeParser : UP.Parser (Route -> a) a
routeParser =
    UP.oneOf
        [ UP.map Work (UP.s "work")
        , UP.map Contact (UP.s "contact")
        , UP.map About UP.top
        ]


toString : Route -> String
toString route =
    case route of
        Work ->
            "#work"

        Contact ->
            "#contact"

        About ->
            ""
