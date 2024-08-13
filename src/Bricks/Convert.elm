module Bricks.Convert exposing (..)

import Bricks.Config as Config
import Bricks.Types exposing (..)
import Point exposing (Point)
import Random
import Time


gridPosToRealPos : Int -> Int -> Point
gridPosToRealPos i j =
    { x = toFloat <| i * (Config.brickWidth + Config.padding)
    , y = toFloat <| j * (Config.brickHeight + Config.padding)
    }


getBrickPosAndRot : Time.Posix -> Brick -> ( Point, Float )
getBrickPosAndRot now brick =
    let
        millisPassedSinceStart =
            Time.posixToMillis now - Time.posixToMillis brick.spawnTime

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
        , interpolateFloat progressFloat originAngle 0
        )


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


interpolateFloat : Float -> Float -> Float -> Float
interpolateFloat f a b =
    (b - a) * f + a
