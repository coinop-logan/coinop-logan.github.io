module BrickWall.Brick exposing (..)

import BrickWall.Common exposing (..)
import BrickWall.Config as Config
import Browser.Dom exposing (Viewport)
import Element
import Point exposing (Point)
import Random
import Responsive exposing (DisplayProfile)
import Time
import Utils


type alias Brick =
    { homePoint : Point
    , state : BrickState
    , gradientUrl : String
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


brickGenerator : Viewport -> ( Int, Int ) -> Bool -> Time.Posix -> Random.Generator Brick
brickGenerator bodyViewport gridPos alreadyPlaced now =
    let
        homePoint =
            gridPosToRealPos (calcBrickDims bodyViewport) gridPos
    in
    if alreadyPlaced then
        Random.constant <|
            makePlacedBrick (calcBrickDims bodyViewport) gridPos

    else
        Random.map
            (\brickState -> Brick homePoint brickState (gradientIdStr gridPos))
            (brickOriginGenerator homePoint |> Random.map (\originInfo -> Moving originInfo now))


makePlacedBrick : BrickDims -> ( Int, Int ) -> Brick
makePlacedBrick brickDims gridPos =
    Brick (gridPosToRealPos brickDims gridPos) Placed (gradientIdStr gridPos)


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


getMovementProgressFloat : Time.Posix -> Brick -> Float
getMovementProgressFloat now brick =
    case brick.state of
        Placed ->
            1

        Moving ( _, _ ) spawnTime ->
            let
                millisPassedSinceStart =
                    Time.posixToMillis now - Time.posixToMillis spawnTime
            in
            -- 0-1 indicates moving; > 1 means the brick has arrived
            toFloat millisPassedSinceStart
                / Config.brickAnimationIntervalMillis
                |> min 1


getBrickPosAndRot : Time.Posix -> Brick -> ( Point, Float )
getBrickPosAndRot now brick =
    case brick.state of
        Placed ->
            ( brick.homePoint, 0 )

        Moving ( originPoint, originAngle ) _ ->
            let
                progressFloat =
                    getMovementProgressFloat now brick
            in
            if progressFloat >= 1 then
                ( brick.homePoint, 0 )

            else
                -- interpolate both linearly based on progressFloat
                ( Point.interpolate progressFloat originPoint brick.homePoint
                , Utils.interpolateFloat progressFloat originAngle 0
                )
