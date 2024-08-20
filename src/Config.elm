module Config exposing (..)

import Time


tabSwitchAnimationInterval : Time.Posix
tabSwitchAnimationInterval =
    Time.millisToPosix 600


animateTabs : Bool
animateTabs =
    True


easingFunctionTruncateMultiplier : Float
easingFunctionTruncateMultiplier =
    0.8
