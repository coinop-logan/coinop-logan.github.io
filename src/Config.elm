module Config exposing (..)

import Element
import Time


bgColor : Element.Color
bgColor =
    Element.rgb 0.1 0.1 0.1


defaultFontColor : Element.Color
defaultFontColor =
    Element.rgb 1 1 1


tabSwitchAnimationInterval : Time.Posix
tabSwitchAnimationInterval =
    Time.millisToPosix 500
