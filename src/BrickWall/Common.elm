module BrickWall.Common exposing (..)

import BrickWall.Config as Config
import Browser.Dom exposing (Viewport)
import Point exposing (Point)
import Random
import Responsive exposing (DisplayProfile)


gridPosToRealPos : BrickDims -> ( Int, Int ) -> Point
gridPosToRealPos ( brickWidth, brickHeight ) ( i, j ) =
    { x =
        toFloat (i * (brickWidth + Config.padding))
            - (if modBy 2 j == 0 then
                toFloat brickWidth / 2

               else
                0.0
              )
            + (Config.padding / 2)
    , y = toFloat (j * (brickHeight + Config.padding)) + Config.padding / 2
    }


realPosToGridPos : BrickDims -> Point -> ( Int, Int )
realPosToGridPos ( brickWidth, brickHeight ) point =
    let
        j =
            floor <| (point.y - (Config.padding / 2)) / toFloat (brickHeight + Config.padding)

        i =
            floor <|
                (point.x
                    - (Config.padding / 2)
                    + (if modBy 2 j == 0 then
                        toFloat brickWidth

                       else
                        0
                      )
                )
                    / toFloat (brickWidth + Config.padding)
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


type alias BrickDims =
    ( Int, Int )


calcBrickDims : Viewport -> ( Int, Int )
calcBrickDims bodyViewport =
    ( calcBrickWidth bodyViewport
    , calcBrickHeight bodyViewport
    )


calcBrickWidth : Viewport -> Int
calcBrickWidth bodyViewport =
    floor bodyViewport.scene.width // Config.numColumns bodyViewport


calcBrickHeight : Viewport -> Int
calcBrickHeight bodyViewport =
    floor <| toFloat (calcBrickWidth bodyViewport) * 0.4


pointToCenterPoint : Viewport -> Point -> Point
pointToCenterPoint bodyViewport p =
    Point.add
        p
        { x = toFloat (calcBrickWidth bodyViewport) / 2
        , y = toFloat (calcBrickHeight bodyViewport) / 2
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
