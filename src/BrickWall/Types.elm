module BrickWall.Types exposing (..)

import Point exposing (Point)
import Random
import Time


type alias Brick =
    { homePoint : Point
    , state : BrickState
    , seed : Random.Seed
    }


type BrickState
    = Placed
    | Moving Time.Posix
