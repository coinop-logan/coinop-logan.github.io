module Bricks.State exposing (..)

import Array2D exposing (Array2D)
import Bricks.Config as Config
import Bricks.Convert as Convert
import Bricks.Types exposing (..)
import Random
import Time


init : Int -> Model
init seedSeed =
    { bricks = Array2D.initialize Config.numBricksAcross Config.numBricksDown (initPlacedBrick seedSeed)
    , seedSeed = seedSeed
    }


initPlacedBrick : Int -> Int -> Int -> Brick
initPlacedBrick seedSeed i j =
    let
        testingSpawnTime =
            seedSeed + (j * 2000)
    in
    { homePoint = Convert.gridPosToRealPos i j
    , spawnTime = Time.millisToPosix testingSpawnTime
    , seed =
        (seedSeed + i + (j * Config.numBricksAcross))
            -- unique input to each brick based on its grid position, which is immutable and unique
            |> Random.initialSeed
    }
