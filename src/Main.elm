module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import State
import Types exposing (..)
import View


main : Program Flags Model Msg
main =
    Browser.document
        { init = State.init
        , view = View.root
        , update = State.update
        , subscriptions = State.subscriptions
        }
