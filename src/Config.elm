module Config exposing (..)

import Responsive exposing (..)
import Time


tabSwitchAnimationInterval : Time.Posix
tabSwitchAnimationInterval =
    Time.millisToPosix 600


animateTabs : Bool
animateTabs =
    False


easingFunctionTruncateMultiplier : Float
easingFunctionTruncateMultiplier =
    0.8


bodyWidth : DisplayProfile -> Float
bodyWidth dProfile =
    responsiveVal dProfile 300.0 800.0


bodyContentWidth : DisplayProfile -> Int
bodyContentWidth dProfile =
    responsiveVal dProfile 330 1050
