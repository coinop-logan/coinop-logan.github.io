module Bricks.Types exposing (..)

import Array2D exposing (Array2D)
import Point exposing (Point)
import Random
import Time


type alias Model =
    { bricks : Array2D Brick
    , seedSeed : Int -- just one ingredient of each brick's seed, thus a seed of a seed
    }


type alias Brick =
    { homePoint : Point
    , spawnTime : Time.Posix
    , seed : Random.Seed
    }
