module Bricks.Types exposing (..)

import Bricks.Wall exposing (BrickWall)
import Point exposing (Point)


type alias Model =
    { bricks : BrickWall
    , seedSeed : Int -- just one ingredient of each brick's seed, thus a seed of a seed
    }
