module BrickWall.Common exposing (..)

import BrickWall.Config as Config
import Point exposing (Point)
import Random


gridPosToRealPos : ( Int, Int ) -> Point
gridPosToRealPos ( i, j ) =
    { x =
        (toFloat <| i * (Config.brickWidth + Config.padding))
            - (if modBy 2 j == 0 then
                Config.brickWidth / 2

               else
                0
              )
            + (Config.padding / 2)
    , y = (toFloat <| j * (Config.brickHeight + Config.padding)) + Config.padding / 2
    }


realPosToGridPos : Point -> ( Int, Int )
realPosToGridPos point =
    let
        j =
            floor <| (point.y - (Config.padding / 2)) / (Config.brickHeight + Config.padding)

        i =
            floor <|
                (point.x
                    - (Config.padding / 2)
                    + (if modBy 2 j == 0 then
                        Config.brickWidth

                       else
                        0
                      )
                )
                    / (Config.brickWidth + Config.padding)
    in
    ( i, j )


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


gradientIdStr : ( Int, Int ) -> String
gradientIdStr ( i, j ) =
    "b" ++ String.fromInt i ++ ":" ++ String.fromInt j
