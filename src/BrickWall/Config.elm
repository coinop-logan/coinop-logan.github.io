module BrickWall.Config exposing (..)

import Element
import Responsive exposing (DisplayProfile, responsiveVal)


brickWidth dProfile =
    responsiveVal dProfile 80 100


brickHeight dProfile =
    responsiveVal dProfile 32 40


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


heightToFillWithCircleGradientPoints : DisplayProfile -> Float
heightToFillWithCircleGradientPoints dProfile =
    responsiveVal dProfile 8000 5000


vSpaceBetweenCircleGradientPoints : DisplayProfile -> Float
vSpaceBetweenCircleGradientPoints dProfile =
    responsiveVal dProfile 500 1000


circleGradientRadius : DisplayProfile -> Float
circleGradientRadius dProfile =
    responsiveVal dProfile 250 700


fadedBrickStrokeOpacity : Float
fadedBrickStrokeOpacity =
    0


brickStrokeOpacity : Float
brickStrokeOpacity =
    0.05


fadedBrickFillOpacity : Float
fadedBrickFillOpacity =
    0.01
