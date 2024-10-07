module BrickWall.Config exposing (..)

import Browser.Dom exposing (Viewport)
import Element
import Responsive exposing (responsiveVal, viewportToDisplayProfile)


numColumns : Viewport -> Int
numColumns bodyViewport =
    -- this is a bit sloppy, because elsewhere we get DisplayProfiles from the full page viewport, which is slightly differently sized due to scrollbar...
    -- however, this only affects a corner case, and the effect itself isn't even that bad
    responsiveVal (viewportToDisplayProfile bodyViewport) 5 10


padding =
    0


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


fadedBrickStrokeOpacity : Float
fadedBrickStrokeOpacity =
    0


brickStrokeOpacity : Float
brickStrokeOpacity =
    0.05


fadedBrickFillOpacity : Float
fadedBrickFillOpacity =
    0.01
