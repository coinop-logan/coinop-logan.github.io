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
    }


initBrick : Time.Posix -> (Random.Seed -> ( Int, Int ) -> ( Random.Seed, Brick ))
initBrick now seed gridPos =
    let
        ( brick, newSeed ) =
            Random.step
                (Brick.brickGenerator gridPos now)
                seed
    in
    ( newSeed, brick )


init : Time.Posix -> Int -> BrickWall
init now numRows =
    let
        masterSeed0 =
            Random.initialSeed (Time.posixToMillis now)

        ( masterSeed1, bricksList ) =
            List.range 0 ((Config.wallWidth * numRows) - 1)
                |> List.map BricksContainer.listPosToGridPos
                |> List.mapAccuml (initBrick now) masterSeed0
    in
    { masterSeed = masterSeed1
    , bricks = BricksContainer.fromList bricksList
    , nameArea = Nothing
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
            initBrick now seed1 chosenPos
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
