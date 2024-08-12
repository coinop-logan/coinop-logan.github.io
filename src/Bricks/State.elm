module Bricks.State exposing (..)

import Array2D exposing (Array2D)
import Bricks.Convert as Convert
import Bricks.Types exposing (..)


init : Model
init =
    { bricks = Array2D.initialize 5 5 initPlacedBrick }


initPlacedBrick : Int -> Int -> Brick
initPlacedBrick i j =
    { homePoint = Convert.gridPosToRealPos i j
    , state = Placed
    }
