module BrickWall.Config exposing (..)

import Element exposing (Color, Element)


brickWidth =
    100


brickHeight =
    40


padding =
    0


brickDefaultStrokeColor =
    Element.rgba 1 1 1 0.2


brickBehindNameStrokeColor =
    Element.rgb 0.15 0.15 0.15


wallWidth : Int
wallWidth =
    20


brickAnimationIntervalMillis : Float
brickAnimationIntervalMillis =
    500


brickTravelAngleAbsMax : Float
brickTravelAngleAbsMax =
    10


brickTravelXAbsMax : Float
brickTravelXAbsMax =
    200


brickTravelYMin : Float
brickTravelYMin =
    600


brickTravelYMax : Float
brickTravelYMax =
    1200


brickMainColor : Element.Color
brickMainColor =
    Element.rgb255 2 10 34


brickGradientColor : Element.Color
brickGradientColor =
    Element.rgb255 46 81 102


heightToFillWithCircleGradientPoints : Float
heightToFillWithCircleGradientPoints =
    5000


vSpaceBetweenCircleGradientPoints : Float
vSpaceBetweenCircleGradientPoints =
    1000


circleGradientRadius : Float
circleGradientRadius =
    700
