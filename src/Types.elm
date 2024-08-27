module Types exposing (..)

import BrickWall.BrickWall as BrickWall exposing (BrickWall)
import Browser
import Browser.Dom exposing (Viewport)
import Browser.Navigation
import CommonTypes exposing (..)
import Responsive exposing (DisplayProfile)
import Route exposing (Route)
import Time
import Url exposing (Url)


type alias Flags =
    ()


type Model
    = Loading LoadingModel
    | Loaded LoadedModel


type alias LoadingModel =
    { viewport : Maybe Viewport
    , time_bySecond : Maybe Time.Posix
    , key : Browser.Navigation.Key
    , url : Url
    }


type alias LoadedModel =
    { key : Browser.Navigation.Key
    , viewport : Viewport
    , route : Route
    , bodyViewport : Maybe Viewport
    , time_bySecond : Time.Posix
    , animateTime : Time.Posix
    , tabState : TabState
    , brickWall : BrickWall
    }


type Msg
    = NoOp
    | GotViewport Viewport
    | GotBodyViewport (Result Browser.Dom.Error Viewport)
    | TriggerGetViewports
    | GotoRoute Route
    | OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url
    | UpdateNow Time.Posix
    | Animate Time.Posix
    | CurrentWorkClicked
    | PortfolioClicked
    | AddBricks Time.Posix
      -- | NameElementSizingInfoGot (Result Browser.Dom.Error Browser.Dom.Element)
    | Test


type Tab
    = CurrentWork
    | PastWork


type TabState
    = OnTab Tab
    | SwitchingTo Tab Time.Posix
