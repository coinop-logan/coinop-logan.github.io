module Convert exposing (..)

import Config
import Time
import Types exposing (..)


animationProgressFloat : Time.Posix -> Time.Posix -> Float
animationProgressFloat startTime now =
    let
        millisElapsed =
            (now |> Time.posixToMillis) - (startTime |> Time.posixToMillis)
    in
    toFloat millisElapsed / (toFloat <| Time.posixToMillis Config.tabSwitchAnimationInterval)


otherTab : Tab -> Tab
otherTab tab =
    case tab of
        CurrentWork ->
            Portfolio

        Portfolio ->
            CurrentWork
