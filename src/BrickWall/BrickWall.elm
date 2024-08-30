module BrickWall.BrickWall exposing (..)

import BrickWall.Brick as Brick exposing (..)
import BrickWall.BricksContainer as BricksContainer exposing (BricksContainer)
import BrickWall.Common exposing (..)
import BrickWall.Config as Config
import List.Extra as List
import Maybe.Extra as Maybe
import Random
import Time


type alias BrickWall =
    { masterSeed : Random.Seed
    , bricks : BricksContainer
    , titleArea : Maybe AreaDef
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
            if targetYPrefill == 0 then
                0

            else
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
    , titleArea = Nothing
    , targetY = targetYPrefill
    }


instantlyPlaceBricksAboveYIfNothing : Float -> BrickWall -> BrickWall
instantlyPlaceBricksAboveYIfNothing y brickWall =
    { brickWall
        | bricks =
            brickWall.bricks
                |> BricksContainer.indexedUpdate
                    (\gridPos maybeBrick ->
                        if (gridPosToRealPos gridPos |> .y) < y then
                            if Maybe.isNothing maybeBrick then
                                Just <| makePlacedBrick gridPos

                            else
                                maybeBrick

                        else
                            maybeBrick
                    )
    }


maybeSpawnNewBricksUnderTargetY : Int -> Time.Posix -> BrickWall -> BrickWall
maybeSpawnNewBricksUnderTargetY howMany now brickWall =
    -- I thought for sure there would be a better way to do this, but I can't find one
    -- I just need to apply a modification function to a type N times...
    -- anyway, all I could figure out was to use List.mapAccuml and just use a dummy list with N length.
    List.mapAccuml
        (\brickWall_ _ ->
            ( maybeSpawnNewBrickUnderTargetY now brickWall_, () )
        )
        brickWall
        (List.repeat howMany ())
        -- ignore list
        |> Tuple.first


maybeSpawnNewBrickUnderTargetY : Time.Posix -> BrickWall -> BrickWall
maybeSpawnNewBrickUnderTargetY now brickWall =
    let
        candidatePositions =
            BricksContainer.getNewBrickCandidatePositions brickWall.targetY brickWall.bricks

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


getYOfFirstNothing : BrickWall -> Float
getYOfFirstNothing brickWall =
    brickWall.bricks
        |> BricksContainer.getFirstGridPosWithNothing
        |> gridPosToRealPos
        |> .y



-- spawnBricksScaledToAmountToFill : Time.Posix -> BrickWall -> BrickWall
-- spawnBricksScaledToAmountToFill now brickWall =
--     let
--         lastBrickY =
--             brickWall.bricks
--                 |> BricksContainer.getLastBrickGridPos
--                 |> Maybe.map gridPosToRealPos
--                 |> Maybe.map .y
--                 |> Maybe.withDefault 0
--         spaceToTargetY =
--             brickWall.targetY - lastBrickY
--         amountToSpawn =
--             if spaceToTargetY <= 0 then
--                 0
--             else
--                 spaceToTargetY / Config.brickHeight |> ceiling
--     in
--     maybeSpawnNewBricksUnderTargetY amountToSpawn now brickWall
