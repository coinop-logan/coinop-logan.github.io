module BrickWall.BricksContainer exposing (..)

import BrickWall.Brick as Brick exposing (Brick)
import BrickWall.Common exposing (..)
import BrickWall.Config as Config
import List.Extra as List
import Maybe.Extra as Maybe
import Responsive exposing (DisplayProfile)
import Time


type BricksContainer
    = Bricks (List (Maybe Brick))



-- initialize : Int -> Int -> (( Int, Int ) -> Brick) -> BricksContainer
-- initialize i j initFunc =
--     Bricks <|
--         List.initialize (i * j)
--             (listPosToGridPos >> initFunc >> Just)


fromList : List Brick -> BricksContainer
fromList bricksList =
    bricksList
        |> List.map Just
        |> Bricks


indexedUpdate : (( Int, Int ) -> Maybe Brick -> Maybe Brick) -> BricksContainer -> BricksContainer
indexedUpdate func (Bricks bricks) =
    List.indexedMap
        (\i -> func (listPosToGridPos i))
        bricks
        |> Bricks


addNewBrickIfNotExists : ( Int, Int ) -> Brick -> BricksContainer -> BricksContainer
addNewBrickIfNotExists gridPos brick (Bricks bricks) =
    case getBrick gridPos bricks of
        Just _ ->
            Bricks bricks

        Nothing ->
            Bricks bricks |> setBrick gridPos brick


setBrick : ( Int, Int ) -> Brick -> BricksContainer -> BricksContainer
setBrick gridPos brick (Bricks bricks) =
    let
        listPos =
            gridPosToListPos gridPos
    in
    Bricks
        (bricks
            |> padListWithNothings (listPos + 1)
            |> List.setAt listPos (Just brick)
        )


toList : BricksContainer -> List (Maybe Brick)
toList (Bricks bricks) =
    bricks


listPosToGridPos : Int -> ( Int, Int )
listPosToGridPos h =
    ( h |> modBy Config.wallWidth
    , h // Config.wallWidth
    )


gridPosToListPos : ( Int, Int ) -> Int
gridPosToListPos ( i, j ) =
    j * Config.wallWidth + i


getNextGridPos : BricksContainer -> ( Int, Int )
getNextGridPos (Bricks bricks) =
    List.length bricks
        |> listPosToGridPos


getNewBrickCandidatePositions : DisplayProfile -> Float -> BricksContainer -> List ( Int, Int )
getNewBrickCandidatePositions dProfile maxY (Bricks bricks) =
    bricks
        |> addEmptyRowIfNeeded
        |> List.findIndices Maybe.isNothing
        |> List.map listPosToGridPos
        |> List.filter (gridPosRealPosIsUnderY dProfile maxY)
        |> List.filter (parentsExist bricks)


gridPosRealPosIsUnderY : DisplayProfile -> Float -> ( Int, Int ) -> Bool
gridPosRealPosIsUnderY dProfile y gridPos =
    (gridPos |> gridPosToRealPos dProfile).y < y


addEmptyRowIfNeeded : List (Maybe Brick) -> List (Maybe Brick)
addEmptyRowIfNeeded bricks =
    let
        numRowsTarget =
            getLastRowWithJust bricks
                + 1
    in
    bricks
        |> padListWithNothings (numRowsTarget * Config.wallWidth)


padListWithNothings : Int -> List (Maybe a) -> List (Maybe a)
padListWithNothings targetLength l =
    if List.length l >= targetLength then
        l

    else
        List.append
            l
            (List.repeat (targetLength - List.length l) Nothing)


getLastRowWithJust : List (Maybe Brick) -> Int
getLastRowWithJust bricks =
    bricks
        -- traverse list in reverse order and find first Just
        |> findLastBrickIndex
        -- turn list index into grid pos, take only row, and default to 0
        |> Maybe.map listPosToGridPos
        |> Maybe.map Tuple.second
        |> Maybe.withDefault 0


getFirstGridPosWithNothing : BricksContainer -> ( Int, Int )
getFirstGridPosWithNothing (Bricks bricks) =
    bricks
        |> List.findIndex Maybe.isNothing
        |> Maybe.map listPosToGridPos
        |> Maybe.withDefault ( 0, 0 )


getLastBrickGridPos : BricksContainer -> Maybe ( Int, Int )
getLastBrickGridPos (Bricks bricks) =
    bricks
        |> findLastBrickIndex
        |> Maybe.map listPosToGridPos


findLastBrickIndex : List (Maybe Brick) -> Maybe Int
findLastBrickIndex bricks =
    bricks
        -- traverse list in reverse order and find first Just
        |> List.reverse
        |> List.findIndex Maybe.isJust
        -- turn "reverse index" into index
        |> Maybe.map (\rh -> List.length bricks - rh)


parentsExist : List (Maybe Brick) -> ( Int, Int ) -> Bool
parentsExist bricks ( i, j ) =
    let
        ( a, b ) =
            getParentsGridPos ( i, j )
    in
    (j <= 0 || i < 0 || i >= Config.wallWidth)
        || (brickExistsAt bricks a && brickExistsAt bricks b)


parentsArePlaced : List (Maybe Brick) -> ( Int, Int ) -> Bool
parentsArePlaced bricks ( i, j ) =
    -- are the "parents" placed?
    let
        ( a, b ) =
            getParentsGridPos ( i, j )
    in
    (j < 0 || i < 0 || i >= Config.wallWidth)
        || (brickIsPlacedAt bricks a && brickIsPlacedAt bricks b)


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


brickExistsAt : List (Maybe Brick) -> ( Int, Int ) -> Bool
brickExistsAt bricks gridPos =
    Maybe.isJust <| getBrick gridPos bricks


brickIsPlacedAt : List (Maybe Brick) -> ( Int, Int ) -> Bool
brickIsPlacedAt bricks gridPos =
    case getBrick gridPos bricks of
        Nothing ->
            False

        Just brick ->
            brick.state == Brick.Placed


getBrick : ( Int, Int ) -> List (Maybe Brick) -> Maybe Brick
getBrick gridPos bricks =
    bricks
        |> List.getAt (gridPosToListPos gridPos)
        |> Maybe.join


updateBricks : (Brick -> Brick) -> BricksContainer -> BricksContainer
updateBricks func (Bricks bricks) =
    Bricks (bricks |> List.map (Maybe.map func))
