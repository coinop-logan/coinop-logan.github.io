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


type alias AreaDef =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


pointToCenterPoint : Point -> Point
pointToCenterPoint p =
    Point.add
        p
        { x = Config.brickWidth / 2
        , y = Config.brickHeight / 2
        }


padArea : Float -> Float -> AreaDef -> AreaDef
padArea x y a =
    { x = a.x - x
    , y = a.y - y
    , width = a.width + (x * 2)
    , height = a.height + (y * 2)
    }
