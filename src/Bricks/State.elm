module Bricks.State exposing (..)

import Bricks.Brick exposing (Brick)
import Bricks.Config as Config
import Bricks.Convert as Convert
import Bricks.Types exposing (..)
import Bricks.Wall as BrickWall exposing (BrickWall)
import Random
import Time


init : Int -> Model
init seedSeed =
    { bricks = BrickWall.initialize Config.wallWidth 1 (initBrick seedSeed)
    , seedSeed = seedSeed
    }


initBrick : Int -> ( Int, Int ) -> Brick
initBrick seedSeed ( i, j ) =
    let
        testingSpawnTime =
            seedSeed + (j * 2000)
    in
    { homePoint = Convert.gridPosToRealPos i j
    , spawnTime = Time.millisToPosix testingSpawnTime
    , seed =
        (seedSeed + i + (j * Config.wallWidth))
            -- unique input to each brick based on its grid position, which is immutable and unique
            |> Random.initialSeed
    }
