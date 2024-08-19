module BrickWall.Brick exposing (..)

import BrickWall.Common exposing (..)
import BrickWall.Config as Config
import Element
import Point exposing (Point)
import Random
import Time
import Utils


type alias Brick =
    { homePoint : Point
    , state : BrickState
    , fillColor : Element.Color
    , strokeColor : Element.Color
    }


type BrickState
    = Placed
    | Moving ( Point, Float ) Time.Posix


updateBrickState : Time.Posix -> Brick -> Brick
updateBrickState now brick =
    case brick.state of
        Placed ->
            brick

        Moving _ spawnTime ->
            if Time.posixToMillis now - Time.posixToMillis spawnTime >= floor Config.brickAnimationIntervalMillis then
                { brick | state = Placed }

            else
                brick


brickGenerator : ( Int, Int ) -> Time.Posix -> Random.Generator Brick
brickGenerator ( i, j ) now =
    let
        homePoint =
            gridPosToRealPos i j
    in
    Random.map2
        (\originInfo fillColor -> Brick homePoint originInfo fillColor Config.brickDefaultStrokeColor)
        (brickOriginGenerator homePoint |> Random.map (\originInfo -> Moving originInfo now))
        brickFillColorGenerator


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

        Moving ( originPoint, originAngle ) spawnTime ->
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
                -- interpolate both linearly based on progressFloat
                ( Point.interpolate progressFloat originPoint brick.homePoint
                , Utils.interpolateFloat progressFloat originAngle 0
                )


brickFillColorGenerator : Random.Generator Element.Color
brickFillColorGenerator =
    Random.map3
        Element.rgb
        (Random.float 0.1 0.2)
        (Random.float 0 0.1)
        (Random.float 0 0.08)



-- brickStrokeColorGenerator : Random.Generator Element.Color
-- brickStrokeColorGenerator =
--     Debug.todo ""
