module Bricks.Brick exposing (..)

import Point exposing (Point)
import Random
import Time


type alias Brick =
    { homePoint : Point
    , spawnTime : Time.Posix
    , seed : Random.Seed
    }
