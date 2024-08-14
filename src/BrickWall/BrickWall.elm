module BrickWall.BrickWall exposing (..)

import BrickWall.Brick exposing (..)
import BrickWall.BricksContainer as BricksContainer exposing (BricksContainer)
import BrickWall.Common exposing (..)
import BrickWall.Config as Config
import Random
import Time


type alias BrickWall =
    { bricks : BricksContainer
    , masterSeed : Random.Seed
    }


init : Time.Posix -> Int -> BrickWall
init now numRows =
    let
        masterSeed0 =
            Random.initialSeed (Time.posixToMillis now)

        ( seedSeed, masterSeed1 ) =
            Random.step seedSeedGenerator masterSeed0
    in
    { bricks = BricksContainer.initialize Config.wallWidth numRows (initBrick seedSeed Placed)
    , masterSeed = masterSeed1
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

        ( seedSeed, seed2 ) =
            Random.step
                seedSeedGenerator
                seed1

        brick =
            initBrick seedSeed (Moving now) chosenPos
    in
    { bricks =
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
