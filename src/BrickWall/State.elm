module BrickWall.State exposing (..)

import BrickWall.BrickWall as BrickWall exposing (BrickWall, Bricks)
import BrickWall.Common exposing (..)
import BrickWall.Config as Config
import BrickWall.Types exposing (..)
import Point exposing (Point)
import Random
import Time
import Utils


init : Time.Posix -> Int -> BrickWall
init now numRows =
    let
        masterSeed0 =
            Random.initialSeed (Time.posixToMillis now)

        ( seedSeed, masterSeed1 ) =
            Random.step (Random.int 0 Random.maxInt) masterSeed0
    in
    { bricks = BrickWall.initialize Config.wallWidth numRows (initBrick seedSeed Placed)
    , masterSeed = masterSeed1
    }


initBrick : Int -> BrickState -> ( Int, Int ) -> Brick
initBrick seedSeed brickState ( i, j ) =
    { homePoint = gridPosToRealPos i j
    , state = brickState
    , seed =
        (seedSeed + i + (j * Config.wallWidth))
            -- unique input to each brick based on its grid position, which is immutable and unique
            |> Random.initialSeed
    }


updateBrickState : Time.Posix -> Brick -> Brick
updateBrickState now brick =
    case brick.state of
        Placed ->
            brick

        Moving spawnTime ->
            if Time.posixToMillis now - Time.posixToMillis spawnTime >= floor Config.brickAnimationIntervalMillis then
                { brick | state = Placed }

            else
                brick


updateBrickStates : Time.Posix -> BrickWall -> BrickWall
updateBrickStates now brickWall =
    { brickWall
        | bricks =
            brickWall.bricks
                |> BrickWall.updateBricks (updateBrickState now)
    }



-- addNewBrick : Int -> Time.Posix -> BrickWall -> BrickWall
-- addNewBrick seedSeed spawnTime wall =
--     wall
--         |> BrickWall.appendBrick
--             (initBrick
--                 seedSeed
--                 spawnTime
--                 (BrickWall.getNextGridPos wall)
--             )


deriveBrickOrigin : Point -> Random.Seed -> ( Point, Float )
deriveBrickOrigin homePoint seed =
    Random.step (brickOriginGenerator homePoint) seed
        -- throw away the stepped seed, we don't need it
        |> Tuple.first


brickOriginGenerator : Point -> Random.Generator ( Point, Float )
brickOriginGenerator homePoint =
    Random.pair
        (originPointGenerator homePoint)
        (Random.float (Config.brickTravelAngleAbsMax * -1) Config.brickTravelAngleAbsMax)


originPointGenerator : Point -> Random.Generator Point
originPointGenerator homePoint =
    Random.map2
        Point
        (Random.float (Config.brickTravelXAbsMax * -1) Config.brickTravelXAbsMax)
        (Random.float Config.brickTravelYMin Config.brickTravelYMax)
        |> Random.map
            (\vec ->
                Point.add homePoint vec
            )


getBrickPosAndRot : Time.Posix -> Brick -> ( Point, Float )
getBrickPosAndRot now brick =
    case brick.state of
        Placed ->
            ( brick.homePoint, 0 )

        Moving spawnTime ->
            let
                millisPassedSinceStart =
                    Time.posixToMillis now - Time.posixToMillis spawnTime

                progressFloat =
                    -- 0-1 indicates moving; > 1 means the brick has arrived
                    toFloat millisPassedSinceStart / Config.brickAnimationIntervalMillis
            in
            if progressFloat >= 1 then
                ( brick.homePoint, 0 )

            else
                let
                    -- derive origin point and angle from seed
                    ( originPoint, originAngle ) =
                        deriveBrickOrigin brick.homePoint brick.seed
                in
                -- interpolate both linearly based on progressFloat
                ( Point.interpolate progressFloat originPoint brick.homePoint
                , Utils.interpolateFloat progressFloat originAngle 0
                )
