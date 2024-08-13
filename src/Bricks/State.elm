module Bricks.State exposing (..)

import Bricks.Brick exposing (Brick)
import Bricks.Config as Config
import Bricks.Convert as Convert
import Bricks.Types exposing (..)
import Bricks.Wall as BrickWall exposing (BrickWall)
import Random
import Time


init : Int -> Int -> Model
init seedSeed numRows =
    { bricks = BrickWall.initialize Config.wallWidth numRows (initBrick seedSeed (Time.millisToPosix 0))
    , seedSeed = seedSeed
    }


initBrick : Int -> Time.Posix -> ( Int, Int ) -> Brick
initBrick seedSeed spawnTime ( i, j ) =
    { homePoint = Convert.gridPosToRealPos i j
    , spawnTime = spawnTime
    , seed =
        (seedSeed + i + (j * Config.wallWidth))
            -- unique input to each brick based on its grid position, which is immutable and unique
            |> Random.initialSeed
    }


addNewBrick : Int -> Time.Posix -> BrickWall -> BrickWall
addNewBrick seedSeed spawnTime wall =
    wall
        |> BrickWall.appendBrick
            (initBrick
                seedSeed
                spawnTime
                (BrickWall.getNextGridPos wall)
            )
