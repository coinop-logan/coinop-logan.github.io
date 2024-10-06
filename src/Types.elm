module Types exposing (..)

import BrickWall.BrickWall as BrickWall exposing (BrickWall)
import Browser
import Browser.Dom exposing (Viewport)
import Browser.Navigation
import CommonTypes exposing (..)
import NymDemo.State as NymDemo
import NymDemo.Types as NymDemo
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
    , brickWall : Maybe BrickWall
    , showContactModal : Bool
    , nymDemoModel : NymDemo.Model
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
    | AddBricks Time.Posix
    | SetShowContactModal Bool
    | NymDemoMsg NymDemo.Msg
    | Test
