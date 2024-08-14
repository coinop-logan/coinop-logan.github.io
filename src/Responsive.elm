module Responsive exposing (..)

import Browser.Dom exposing (Viewport)


type DisplayProfile
    = Desktop
    | Mobile


viewportToDisplayProfile : Viewport -> DisplayProfile
viewportToDisplayProfile =
    .scene >> .width >> screenWidthToDisplayProfile


screenWidthToDisplayProfile : number -> DisplayProfile
screenWidthToDisplayProfile width =
    if width >= 1150 then
        Desktop

    else
        Mobile


responsiveVal : DisplayProfile -> a -> a -> a
responsiveVal dProfile mobileVal desktopVal =
    case dProfile of
        Mobile ->
            mobileVal

        Desktop ->
            desktopVal
