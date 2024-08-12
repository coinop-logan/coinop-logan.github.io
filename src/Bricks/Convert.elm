module Bricks.Convert exposing (..)

import Bricks.Config as Config
import Bricks.Types exposing (..)
import Point exposing (Point)


gridPosToRealPos : Int -> Int -> Point
gridPosToRealPos i j =
    { x = toFloat <| i * (Config.brickWidth + Config.padding)
    , y = toFloat <| j * (Config.brickHeight + Config.padding)
    }
