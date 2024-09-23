module NymDemo.Config exposing (..)

import Responsive exposing (DisplayProfile(..), responsiveVal)


numNyms =
    4


nymDemoRenderDimensions : DisplayProfile -> ( Int, Int )
nymDemoRenderDimensions dProfile =
    responsiveVal dProfile ( 320, 320 ) ( 1000, 200 )


cameraHeight : DisplayProfile -> Float
cameraHeight dProfile =
    responsiveVal dProfile 10 5
