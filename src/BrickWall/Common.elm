module BrickWall.Common exposing (..)

import BrickWall.Config as Config
import Point exposing (Point)
import Random


gridPosToRealPos : Int -> Int -> Point
gridPosToRealPos i j =
    { x =
        (toFloat <| i * (Config.brickWidth + Config.padding))
            - (if modBy 2 j == 0 then
                Config.brickWidth / 2

               else
                0
              )
            + (Config.padding / 2)
    , y = toFloat <| j * (Config.brickHeight + Config.padding) + Config.padding // 2
    }


seedSeedGenerator : Random.Generator Int
seedSeedGenerator =
    Random.int 0 Random.maxInt
