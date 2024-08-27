module BrickWall.Config exposing (..)

import Element exposing (Color, Element)


brickWidth =
    100


brickHeight =
    40


padding =
    0


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


fadedBrickStrokeOpacity : Float
fadedBrickStrokeOpacity =
    0


brickStrokeOpacity : Float
brickStrokeOpacity =
    0.05


fadedBrickFillOpacity : Float
fadedBrickFillOpacity =
    0.01
