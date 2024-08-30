module Route exposing (..)

import Url exposing (Url)
import Url.Builder as UB
import Url.Parser as UP


type Route
    = Projects
    | About


parseUrl : Url -> Route
parseUrl url =
    UP.parse routeParser url
        |> Maybe.withDefault About


routeParser : UP.Parser (Route -> a) a
routeParser =
    UP.oneOf
        [ UP.map Projects (UP.s "projects")
        , UP.map About UP.top
        ]


toString : Route -> String
toString route =
    case route of
        Projects ->
            "#projects/"

        About ->
            "/"
