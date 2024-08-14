module Types exposing (..)

import BrickWall.BrickWall as BrickWall exposing (BrickWall)
import Browser.Dom exposing (Viewport)
import CommonTypes exposing (..)
import Responsive exposing (DisplayProfile)
import Time


type alias Flags =
    ()


type Model
    = Loading LoadingModel
    | Loaded LoadedModel


type alias LoadingModel =
    { viewport : Maybe Viewport
    , time_bySecond : Maybe Time.Posix
    }


type alias LoadedModel =
    { viewport : Viewport
    , time_bySecond : Time.Posix
    , animateTime : Time.Posix
    , tabState : TabState
    , brickWall : BrickWall
    }


type Msg
    = NoOp
    | GotViewport Viewport
    | TriggerGetViewport
    | UpdateNow Time.Posix
    | Animate Time.Posix
    | CurrentWorkClicked
    | PortfolioClicked
    | TestBrickShit Time.Posix


type Tab
    = CurrentWork
    | Portfolio


type TabState
    = OnTab Tab
    | SwitchingTo Tab Time.Posix
