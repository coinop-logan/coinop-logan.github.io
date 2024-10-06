module BrickWall.BricksContainer exposing (..)

import BrickWall.Brick as Brick exposing (Brick)
import BrickWall.Common exposing (..)
import BrickWall.Config as Config
import Browser.Dom exposing (Viewport)
import List.Extra as List
import Maybe.Extra as Maybe
import Responsive exposing (DisplayProfile)
import Time


type alias BricksContainer =
    { numColumns : Int
    , bricks : List (Maybe Brick)
    }


init : Int -> List Brick -> BricksContainer
init numColumns bricksList =
    { numColumns = numColumns
    , bricks =
        bricksList
            |> List.map Just
    }


indexedUpdate : (( Int, Int ) -> Maybe Brick -> Maybe Brick) -> BricksContainer -> BricksContainer
indexedUpdate func container =
    { container
        | bricks =
            List.indexedMap
                (\i -> func (listPosToGridPos container.numColumns i))
                container.bricks
    }


addNewBrickIfNotExists : ( Int, Int ) -> Brick -> BricksContainer -> BricksContainer
addNewBrickIfNotExists gridPos brick container =
    case getBrick container gridPos of
        Just _ ->
            container

        Nothing ->
            container |> setBrick gridPos brick


setBrick : ( Int, Int ) -> Brick -> BricksContainer -> BricksContainer
setBrick gridPos brick container =
    let
        listPos =
            gridPosToListPos container gridPos
    in
    { container
        | bricks =
            container.bricks
                |> padListWithNothings (listPos + 1)
                |> List.setAt listPos (Just brick)
    }


toList : BricksContainer -> List (Maybe Brick)
toList container =
    container.bricks


listPosToGridPos : Int -> Int -> ( Int, Int )
listPosToGridPos numColumns h =
    ( h |> modBy numColumns
    , h // numColumns
    )


gridPosToListPos : BricksContainer -> ( Int, Int ) -> Int
gridPosToListPos container ( i, j ) =
    j * container.numColumns + i


getNextGridPos : BricksContainer -> ( Int, Int )
getNextGridPos container =
    List.length container.bricks
        |> listPosToGridPos container.numColumns


getNewBrickCandidatePositions : Viewport -> Float -> BricksContainer -> List ( Int, Int )
getNewBrickCandidatePositions bodyViewport maxY container =
    let
        paddedContainer =
            container
                |> addEmptyRowIfNeeded
    in
    paddedContainer.bricks
        |> List.findIndices Maybe.isNothing
        |> List.map (listPosToGridPos paddedContainer.numColumns)
        |> List.filter (gridPosRealPosIsUnderY bodyViewport maxY)
        |> List.filter (parentsExist container)


gridPosRealPosIsUnderY : Viewport -> Float -> ( Int, Int ) -> Bool
gridPosRealPosIsUnderY bodyViewport y gridPos =
    (gridPos |> gridPosToRealPos (calcBrickDims bodyViewport)).y < y


addEmptyRowIfNeeded : BricksContainer -> BricksContainer
addEmptyRowIfNeeded container =
    let
        numRowsTarget =
            getLastRowWithJust container
                + 1
    in
    { container
        | bricks =
            container.bricks
                |> padListWithNothings (numRowsTarget * container.numColumns)
    }


padListWithNothings : Int -> List (Maybe a) -> List (Maybe a)
padListWithNothings targetLength l =
    if List.length l >= targetLength then
        l

    else
        List.append
            l
            (List.repeat (targetLength - List.length l) Nothing)


getLastRowWithJust : BricksContainer -> Int
getLastRowWithJust container =
    container.bricks
        -- traverse list in reverse order and find first Just
        |> findLastBrickIndex
        -- turn list index into grid pos, take only row, and default to 0
        |> Maybe.map (listPosToGridPos container.numColumns)
        |> Maybe.map Tuple.second
        |> Maybe.withDefault 0


getFirstGridPosWithNothing : BricksContainer -> ( Int, Int )
getFirstGridPosWithNothing container =
    container.bricks
        |> List.findIndex Maybe.isNothing
        |> Maybe.map (listPosToGridPos container.numColumns)
        |> Maybe.withDefault ( 0, 0 )


getLastBrickGridPos : BricksContainer -> Maybe ( Int, Int )
getLastBrickGridPos container =
    container.bricks
        |> findLastBrickIndex
        |> Maybe.map (listPosToGridPos container.numColumns)


findLastBrickIndex : List (Maybe Brick) -> Maybe Int
findLastBrickIndex bricks =
    bricks
        -- traverse list in reverse order and find first Just
        |> List.reverse
        |> List.findIndex Maybe.isJust
        -- turn "reverse index" into index
        |> Maybe.map (\rh -> List.length bricks - rh)


parentsExist : BricksContainer -> ( Int, Int ) -> Bool
parentsExist container ( i, j ) =
    let
        ( a, b ) =
            getParentsGridPos ( i, j )
    in
    (j <= 0 || i < 0 || i >= container.numColumns)
        || (brickExistsAt container a && brickExistsAt container b)


parentsArePlaced : BricksContainer -> ( Int, Int ) -> Bool
parentsArePlaced container ( i, j ) =
    let
        ( a, b ) =
            getParentsGridPos ( i, j )
    in
    (j < 0 || i < 0 || i >= container.numColumns)
        || (brickIsPlacedAt container a && brickIsPlacedAt container b)


getParentsGridPos : ( Int, Int ) -> ( ( Int, Int ), ( Int, Int ) )
getParentsGridPos ( i, j ) =
    let
        ( i1, i2 ) =
            if modBy 2 j == 0 then
                ( i - 1, i )

            else
                ( i, i + 1 )
    in
    ( ( i1, j - 1 )
    , ( i2, j - 1 )
    )


brickExistsAt : BricksContainer -> ( Int, Int ) -> Bool
brickExistsAt container gridPos =
    Maybe.isJust <| getBrick container gridPos


brickIsPlacedAt : BricksContainer -> ( Int, Int ) -> Bool
brickIsPlacedAt container gridPos =
    case getBrick container gridPos of
        Nothing ->
            False

        Just brick ->
            brick.state == Brick.Placed


getBrick : BricksContainer -> ( Int, Int ) -> Maybe Brick
getBrick container gridPos =
    container.bricks
        |> List.getAt (gridPosToListPos container gridPos)
        |> Maybe.join


updateBricks : (Brick -> Brick) -> BricksContainer -> BricksContainer
updateBricks func container =
    { container
        | bricks =
            container.bricks |> List.map (Maybe.map func)
    }
