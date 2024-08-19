module BrickWall.BrickWall exposing (..)

import BrickWall.Brick as Brick exposing (..)
import BrickWall.BricksContainer as BricksContainer exposing (BricksContainer)
import BrickWall.Common exposing (..)
import BrickWall.Config as Config
import List.Extra as List
import Random
import Time


type alias BrickWall =
    { masterSeed : Random.Seed
    , bricks : BricksContainer
    , nameArea : Maybe AreaDef
    , targetY : Float
    }


initBrick : Time.Posix -> Bool -> (Random.Seed -> ( Int, Int ) -> ( Random.Seed, Brick ))
initBrick now alreadyPlaced seed gridPos =
    let
        ( brick, newSeed ) =
            Random.step
                (Brick.brickGenerator gridPos alreadyPlaced now)
                seed
    in
    ( newSeed, brick )


init : Time.Posix -> Float -> BrickWall
init now targetYPrefill =
    let
        initialNeededRows =
            (realPosToGridPos { x = 0, y = targetYPrefill }
                |> Tuple.second
            )
                + 1

        masterSeed0 =
            Random.initialSeed (Time.posixToMillis now)

        ( masterSeed1, bricksList ) =
            List.range 0 ((Config.wallWidth * initialNeededRows) - 1)
                |> List.map BricksContainer.listPosToGridPos
                |> List.mapAccuml (initBrick now True) masterSeed0
    in
    { masterSeed = masterSeed1
    , bricks = BricksContainer.fromList bricksList
    , nameArea = Nothing
    , targetY = targetYPrefill
    }


spawnNewBrick : Time.Posix -> BrickWall -> BrickWall
spawnNewBrick now brickWall =
    let
        candidatePositions =
            BricksContainer.getNewBrickCandidatePositions brickWall.bricks

        ( chosenPos, seed1 ) =
            case candidatePositions of
                [] ->
                    ( ( 0, 0 ), brickWall.masterSeed )

                x :: xs ->
                    Random.step
                        (Random.uniform x xs)
                        brickWall.masterSeed

        ( seed2, brick ) =
            initBrick now False seed1 chosenPos
    in
    { brickWall
        | bricks =
            brickWall.bricks
                |> BricksContainer.addNewBrickIfNotExists chosenPos brick
        , masterSeed = seed2
    }


updateBrickStates : Time.Posix -> BrickWall -> BrickWall
updateBrickStates now brickWall =
    { brickWall
        | bricks =
            brickWall.bricks
                |> BricksContainer.updateBricks (updateBrickState now)
    }


addBrickIfTargetYNotMet : Time.Posix -> BrickWall -> BrickWall
addBrickIfTargetYNotMet now brickWall =
    let
        lastPlacedY =
            BricksContainer.getLastBrickGridPos brickWall.bricks
                |> Maybe.map gridPosToRealPos
                |> Maybe.map .y
                |> Maybe.withDefault 0
    in
    if lastPlacedY < brickWall.targetY then
        brickWall
            |> updateBrickStates now
            |> spawnNewBrick now

    else
        brickWall
