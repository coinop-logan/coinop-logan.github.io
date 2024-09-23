module BrickWall.Common exposing (..)

import BrickWall.Config as Config
import Point exposing (Point)
import Random
import Responsive exposing (DisplayProfile)


gridPosToRealPos : DisplayProfile -> ( Int, Int ) -> Point
gridPosToRealPos dProfile ( i, j ) =
    { x =
        (toFloat <| i * (Config.brickWidth dProfile + Config.padding))
            - (if modBy 2 j == 0 then
                Config.brickWidth dProfile / 2

               else
                0
              )
            + (Config.padding / 2)
    , y = (toFloat <| j * (Config.brickHeight dProfile + Config.padding)) + Config.padding / 2
    }


realPosToGridPos : DisplayProfile -> Point -> ( Int, Int )
realPosToGridPos dProfile point =
    let
        j =
            floor <| (point.y - (Config.padding / 2)) / (Config.brickHeight dProfile + Config.padding)

        i =
            floor <|
                (point.x
                    - (Config.padding / 2)
                    + (if modBy 2 j == 0 then
                        Config.brickWidth dProfile

                       else
                        0
                      )
                )
                    / (Config.brickWidth dProfile + Config.padding)
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


pointToCenterPoint : DisplayProfile -> Point -> Point
pointToCenterPoint dProfile p =
    Point.add
        p
        { x = Config.brickWidth dProfile / 2
        , y = Config.brickHeight dProfile / 2
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
