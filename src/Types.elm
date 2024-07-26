module Types exposing (..)

import Browser.Dom
import Responsive exposing (DisplayProfile)
import Time


type alias Flags =
    ()


type Model
    = Loading LoadingModel
    | Loaded LoadedModel


type alias LoadingModel =
    { dProfile : Maybe DisplayProfile
    , time_bySecond : Maybe Time.Posix
    }


type alias LoadedModel =
    { dProfile : DisplayProfile
    , time_bySecond : Time.Posix
    }


type Msg
    = NoOp
    | GotViewport Browser.Dom.Viewport
    | UpdateNow Time.Posix
