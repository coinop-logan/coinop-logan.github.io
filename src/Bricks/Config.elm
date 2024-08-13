module Bricks.Config exposing (..)

import Element exposing (Color, Element)


brickWidth =
    50


brickHeight =
    20


padding =
    5


brickFillColor =
    Element.rgb 0.1 0.1 0.1


brickStrokeColor =
    Element.rgb 0.8 0.8 0.8


wallWidth : Int
wallWidth =
    20


brickAnimationIntervalMillis : Float
brickAnimationIntervalMillis =
    6000


brickTravelAngleAbsMax : Float
brickTravelAngleAbsMax =
    20


brickTravelXAbsMax : Float
brickTravelXAbsMax =
    200


brickTravelYMin : Float
brickTravelYMin =
    300


brickTravelYMax : Float
brickTravelYMax =
    700
