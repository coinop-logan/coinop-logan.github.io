module Types exposing (..)

import BrickWall.BrickWall as BrickWall exposing (BrickWall)
import Browser.Dom
import CommonTypes exposing (..)
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
    , animateTime : Time.Posix
    , tabState : TabState
    , brickWall : BrickWall
    }


type Msg
    = NoOp
    | GotViewport Browser.Dom.Viewport
    | Resize Int Int
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
