module NymDemo.Common exposing (..)

import Color exposing (Color)
import List.Extra as List
import Nym exposing (BinarySource, NymTemplate)
import TupleHelpers
import Utils
import Vector3 exposing (Vector3)


interpolateNymsForRendering : Float -> NymTemplate -> NymTemplate -> NymTemplate
interpolateNymsForRendering interp start end =
    { structure =
        { eyeQuadInfo =
            Result.map2
                (\startEQInfo endEQInfo ->
                    { sketchPlane =
                        --can ignore, not used in rendering
                        startEQInfo.sketchPlane
                    , eyeQuad =
                        { bottomRight = Vector3.interpolate interp startEQInfo.eyeQuad.bottomRight endEQInfo.eyeQuad.bottomRight
                        , bottomLeft = Vector3.interpolate interp startEQInfo.eyeQuad.bottomLeft endEQInfo.eyeQuad.bottomLeft
                        , topLeft = Vector3.interpolate interp startEQInfo.eyeQuad.topLeft endEQInfo.eyeQuad.topLeft
                        , topRight = Vector3.interpolate interp startEQInfo.eyeQuad.topRight endEQInfo.eyeQuad.topRight
                        }
                    , pupil = interpolatePupil interp startEQInfo.pupil endEQInfo.pupil
                    }
                )
                start.structure.eyeQuadInfo
                end.structure.eyeQuadInfo
        , noseTop =
            Result.map2 (Vector3.interpolate interp) start.structure.noseTop end.structure.noseTop
        , noseBridge =
            Result.map2 (Vector3.interpolate interp) start.structure.noseBridge end.structure.noseBridge
        , noseBottom =
            Result.map2 (Vector3.interpolate interp) start.structure.noseBottom end.structure.noseBottom
        , cheekbone =
            Result.map2 (Vector3.interpolate interp) start.structure.cheekbone end.structure.cheekbone
        , crownFront =
            Result.map2 (Vector3.interpolate interp) start.structure.crownFront end.structure.crownFront
        , crownBack =
            Result.map2 (Vector3.interpolate interp) start.structure.crownBack end.structure.crownBack
        , backZ =
            Result.map2 (Utils.interpolateFloat interp) start.structure.backZ end.structure.backZ
        , faceSideTop =
            Result.map2 (Vector3.interpolate interp) start.structure.faceSideTop end.structure.faceSideTop
        , faceSideMid =
            Result.map2 (Vector3.interpolate interp) start.structure.faceSideMid end.structure.faceSideMid
        , faceSideBottom =
            Result.map2 (Vector3.interpolate interp) start.structure.faceSideBottom end.structure.faceSideBottom
        , jawPoint =
            Result.map2 (Vector3.interpolate interp) start.structure.jawPoint end.structure.jawPoint
        , chin =
            Result.map2 (Vector3.interpolate interp) start.structure.chin end.structure.chin
        , earAttachFrontTop =
            Result.map2 (Vector3.interpolate interp) start.structure.earAttachFrontTop end.structure.earAttachFrontTop
        , earAttachFrontBottom =
            Result.map2 (Vector3.interpolate interp) start.structure.earAttachFrontBottom end.structure.earAttachFrontBottom
        , earBaseNormal =
            -- can ignore, not used in rendering
            start.structure.earBaseNormal
        , earAttachBack =
            Result.map2 (Vector3.interpolate interp) start.structure.earAttachBack end.structure.earAttachBack
        , earAttachInside =
            Result.map2 (Vector3.interpolate interp) start.structure.earAttachInside end.structure.earAttachInside
        , earTip =
            Result.map2 (Vector3.interpolate interp) start.structure.earTip end.structure.earTip
        }
    , coloring =
        { snoutTop =
            Result.map2 (interpolateColors interp) start.coloring.snoutTop end.coloring.snoutTop
        , snoutSideTopMajor =
            Result.map2 (interpolateColors interp) start.coloring.snoutSideTopMajor end.coloring.snoutSideTopMajor
        , snoutSideTopMinor =
            Result.map2 (interpolateColors interp) start.coloring.snoutSideTopMinor end.coloring.snoutSideTopMinor
        , snoutSideMiddle =
            Result.map2 (interpolateColors interp) start.coloring.snoutSideMiddle end.coloring.snoutSideMiddle
        , noseTip =
            Result.map2 (interpolateColors interp) start.coloring.noseTip end.coloring.noseTip
        , aboveCheekbone =
            Result.map2 (interpolateColors interp) start.coloring.aboveCheekbone end.coloring.aboveCheekbone
        , bridge =
            Result.map2 (interpolateColors interp) start.coloring.bridge end.coloring.bridge
        , forehead =
            Result.map2 (interpolateColors interp) start.coloring.forehead end.coloring.forehead
        , aboveEye =
            Result.map2 (interpolateColors interp) start.coloring.aboveEye end.coloring.aboveEye
        , eyeQuad =
            Result.map2 (interpolateColors interp) start.coloring.eyeQuad end.coloring.eyeQuad
        , belowEar =
            Result.map2 (interpolateColors interp) start.coloring.belowEar end.coloring.belowEar
        , faceSideTop =
            Result.map2 (interpolateColors interp) start.coloring.faceSideTop end.coloring.faceSideTop
        , faceSideBottom =
            Result.map2 (interpolateColors interp) start.coloring.faceSideBottom end.coloring.faceSideBottom
        , snoutSideBottom =
            Result.map2 (interpolateColors interp) start.coloring.snoutSideBottom end.coloring.snoutSideBottom
        , jawSide =
            Result.map2 (interpolateColors interp) start.coloring.jawSide end.coloring.jawSide
        , mouth =
            Result.map2 (interpolateColors interp) start.coloring.mouth end.coloring.mouth
        , chinBottom =
            Result.map2 (interpolateColors interp) start.coloring.chinBottom end.coloring.chinBottom
        , neck =
            Result.map2 (interpolateColors interp) start.coloring.neck end.coloring.neck
        , crown =
            Result.map2 (interpolateColors interp) start.coloring.crown end.coloring.crown
        , crownSide =
            Result.map2 (interpolateColors interp) start.coloring.crownSide end.coloring.crownSide
        , earBackOuter =
            Result.map2 (interpolateColors interp) start.coloring.earBackOuter end.coloring.earBackOuter
        , earBackInner =
            Result.map2 (interpolateColors interp) start.coloring.earBackInner end.coloring.earBackInner
        , earFrontOuter =
            Result.map2 (interpolateColors interp) start.coloring.earFrontOuter end.coloring.earFrontOuter
        , earFrontInner =
            Result.map2 (interpolateColors interp) start.coloring.earFrontInner end.coloring.earFrontInner
        }
    }


interpolatePupil : Float -> List ( Vector3, Vector3, Vector3 ) -> List ( Vector3, Vector3, Vector3 ) -> List ( Vector3, Vector3, Vector3 )
interpolatePupil interp pupil1 pupil2 =
    if interp == 0 then
        pupil1

    else if interp == 1 then
        pupil2

    else
        let
            ( modifiedPupil1, modifiedPupil2 ) =
                if List.length pupil1 < List.length pupil2 then
                    ( pupil1
                        |> List.cycle (List.length pupil2)
                    , pupil2
                    )

                else if List.length pupil1 > List.length pupil2 then
                    ( pupil1
                    , pupil2
                        |> List.cycle (List.length pupil1)
                    )

                else
                    ( pupil1, pupil2 )
        in
        List.zip
            modifiedPupil1
            modifiedPupil2
            |> List.map
                (\( vectorTupleA, vectorTupleB ) ->
                    TupleHelpers.mergeTuple3
                        ( Vector3.interpolate interp
                        , Vector3.interpolate interp
                        , Vector3.interpolate interp
                        )
                        vectorTupleA
                        vectorTupleB
                )


interpolateColors : Float -> Color -> Color -> Color
interpolateColors interp c1 c2 =
    let
        ( c1Rgba, c2Rgba ) =
            ( c1, c2 )
                |> TupleHelpers.mapTuple2 Color.toRgba
    in
    Color.fromRgba
        { red = Utils.interpolateFloat interp c1Rgba.red c2Rgba.red
        , green = Utils.interpolateFloat interp c1Rgba.green c2Rgba.green
        , blue = Utils.interpolateFloat interp c1Rgba.blue c2Rgba.blue
        , alpha = 1
        }


genNymTemplate : BinarySource -> NymTemplate
genNymTemplate source =
    source
        |> Nym.binarySourceToNymTemplate
        |> TupleHelpers.tuple3Last
