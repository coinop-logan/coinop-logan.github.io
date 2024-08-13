module Bricks.Wall exposing (..)

import Bricks.Brick exposing (Brick)
import Bricks.Config as Config
import List.Extra as List


type BrickWall
    = BrickWall (List Brick)


initialize : Int -> Int -> (( Int, Int ) -> Brick) -> BrickWall
initialize i j initFunc =
    BrickWall <|
        List.initialize (i * j)
            (listPosToGridPos >> initFunc)


toList : BrickWall -> List Brick
toList (BrickWall brickWall) =
    brickWall


listPosToGridPos : Int -> ( Int, Int )
listPosToGridPos h =
    ( h |> modBy Config.wallWidth
    , h // Config.wallWidth
    )


gridPosToListPos : ( Int, Int ) -> Int
gridPosToListPos ( i, j ) =
    j * Config.wallWidth + i
