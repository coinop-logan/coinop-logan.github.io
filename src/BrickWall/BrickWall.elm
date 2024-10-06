module BrickWall.BrickWall exposing (..)

import BrickWall.Brick as Brick exposing (..)
import BrickWall.BricksContainer as BricksContainer exposing (BricksContainer)
import BrickWall.Common exposing (..)
import BrickWall.Config as Config
import Browser.Dom exposing (Viewport)
import List.Extra as List
import Maybe.Extra as Maybe
import Random
import Responsive exposing (DisplayProfile)
import Time


type alias BrickWall =
    { masterSeed : Random.Seed
    , bricks : BricksContainer
    , titleArea : Maybe AreaDef
    , bodyViewport : Viewport
    }


initBrick : Viewport -> Time.Posix -> Bool -> (Random.Seed -> ( Int, Int ) -> ( Random.Seed, Brick ))
initBrick bodyViewport now alreadyPlaced seed gridPos =
    let
        ( brick, newSeed ) =
            Random.step
                (Brick.brickGenerator bodyViewport gridPos alreadyPlaced now)
                seed
    in
    ( newSeed, brick )


init : Viewport -> Time.Posix -> BrickWall
init bodyViewport now =
    let
        masterSeed0 =
            Random.initialSeed (Time.posixToMillis now)

        numColumns =
            bodyViewport.scene.width
                / toFloat (calcBrickWidth bodyViewport)
                |> floor
                |> (+) 1

        numRowsPrefill =
            (bodyViewport.viewport.y + bodyViewport.viewport.height)
                / toFloat (calcBrickHeight bodyViewport)
                |> floor

        ( masterSeed1, bricksList ) =
            List.range 0 ((numColumns * numRowsPrefill) - 1)
                |> List.map (BricksContainer.listPosToGridPos numColumns)
                |> List.mapAccuml (initBrick bodyViewport now True) masterSeed0
    in
    { masterSeed = masterSeed1
    , bricks = BricksContainer.init numColumns bricksList
    , titleArea = Nothing
    , bodyViewport = bodyViewport
    }


instantlyPlaceBricksAboveYIfNothing : Float -> BrickWall -> BrickWall
instantlyPlaceBricksAboveYIfNothing y brickWall =
    { brickWall
        | bricks =
            brickWall.bricks
                |> BricksContainer.indexedUpdate
                    (\gridPos maybeBrick ->
                        if (gridPosToRealPos (calcBrickDims brickWall.bodyViewport) gridPos |> .y) < y then
                            if Maybe.isNothing maybeBrick then
                                Just <| makePlacedBrick (calcBrickDims brickWall.bodyViewport) gridPos

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
            BricksContainer.getNewBrickCandidatePositions brickWall.bodyViewport (getTargetY brickWall) brickWall.bricks

        ( chosenPos, seed1 ) =
            case candidatePositions of
                [] ->
                    ( ( 0, 0 ), brickWall.masterSeed )

                x :: xs ->
                    Random.step
                        (Random.uniform x xs)
                        brickWall.masterSeed

        ( seed2, brick ) =
            initBrick brickWall.bodyViewport now False seed1 chosenPos
    in
    { brickWall
        | bricks =
            brickWall.bricks
                |> BricksContainer.addNewBrickIfNotExists chosenPos brick
        , masterSeed = seed2
    }


getTargetY : BrickWall -> Float
getTargetY brickWall =
    brickWall.bodyViewport.viewport.y + brickWall.bodyViewport.viewport.height


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
        |> gridPosToRealPos (calcBrickDims brickWall.bodyViewport)
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
