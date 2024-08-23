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


pastWorkTabBottmY : DisplayProfile -> Float
pastWorkTabBottmY dProfile =
    case dProfile of
        Desktop ->
            2250

        Mobile ->
            4080


currentWorkTabBottomY : DisplayProfile -> Float
currentWorkTabBottomY dProfile =
    case dProfile of
        Desktop ->
            2200

        Mobile ->
            2350


bodyWidth : DisplayProfile -> Float
bodyWidth dProfile =
    responsiveVal dProfile 300.0 800.0
