module BrickWall.BrickWall exposing (..)

-- import BrickWall.Types exposing (..)

import BrickWall.Config as Config
import BrickWall.Types exposing (..)
import List.Extra as List
import Maybe.Extra as Maybe
import Random


type alias BrickWall =
    { bricks : Bricks
    , masterSeed : Random.Seed
    }


type Bricks
    = Bricks (List (Maybe Brick))


initialize : Int -> Int -> (( Int, Int ) -> Brick) -> Bricks
initialize i j initFunc =
    Bricks <|
        List.initialize (i * j)
            (listPosToGridPos >> initFunc >> Just)


toList : Bricks -> List (Maybe Brick)
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


getNextGridPos : Bricks -> ( Int, Int )
getNextGridPos (Bricks bricks) =
    List.length bricks
        |> listPosToGridPos


getNewBrickCandidatePositions : Bricks -> List ( Int, Int )
getNewBrickCandidatePositions (Bricks bricks) =
    bricks
        |> addEmptyRowIfNeeded
        |> List.findIndices Maybe.isNothing
        |> List.map listPosToGridPos
        |> List.filter (parentsArePlaced bricks)


addEmptyRowIfNeeded : List (Maybe Brick) -> List (Maybe Brick)
addEmptyRowIfNeeded bricks =
    let
        numRowsTarget =
            getLastRowWithJust bricks + 1

        listLengthTarget =
            Debug.log "t" <|
                numRowsTarget
                    * Config.wallWidth
    in
    if List.length bricks >= listLengthTarget then
        bricks

    else
        List.append
            bricks
            (List.repeat (listLengthTarget - List.length bricks) Nothing)


getLastRowWithJust : List (Maybe Brick) -> Int
getLastRowWithJust bricks =
    bricks
        -- traverse list in reverse order and find first Just
        |> List.reverse
        |> List.findIndex Maybe.isJust
        -- turn "reverse index" into index
        |> Maybe.map (\rh -> List.length bricks - rh)
        -- turn list index into grid pos, take only row, and default to 0
        |> Maybe.map listPosToGridPos
        |> Maybe.map Tuple.second
        |> Maybe.withDefault 0


parentsArePlaced : List (Maybe Brick) -> ( Int, Int ) -> Bool
parentsArePlaced bricks ( i, j ) =
    if j == 0 then
        True

    else
        -- are the "parents" placed?
        let
            ( a, b ) =
                getParentsGridPos ( i, j )
        in
        brickIsPlacedAt bricks a && brickIsPlacedAt bricks b


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


brickIsPlacedAt : List (Maybe Brick) -> ( Int, Int ) -> Bool
brickIsPlacedAt bricks gridPos =
    case getBrick bricks gridPos of
        Nothing ->
            False

        Just brick ->
            brick.state == Placed


getBrick : List (Maybe Brick) -> ( Int, Int ) -> Maybe Brick
getBrick bricks gridPos =
    bricks
        |> List.getAt (gridPosToListPos gridPos)
        |> Maybe.join


updateBricks : (Brick -> Brick) -> Bricks -> Bricks
updateBricks func (Bricks bricks) =
    Bricks (bricks |> List.map (Maybe.map func))
