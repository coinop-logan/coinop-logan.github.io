module Bricks.Types exposing (..)

import Array2D exposing (Array2D)
import Point exposing (Point)
import Time


type alias Model =
    { bricks : Array2D Brick }


type alias Brick =
    { homePoint : Point
    , state : BrickState
    }


type BrickState
    = Placed
    | Moving MovingBrickState


type alias MovingBrickState =
    { moveStartTime : Time.Posix
    , startPoint : Point
    , startAngle : Float
    , intermediatePoint : Point
    }
