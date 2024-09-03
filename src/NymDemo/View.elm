module NymDemo.View exposing (..)

import Angle
import Axis3d
import Camera3d
import Color exposing (Color)
import Direction3d
import Element exposing (Element)
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Length
import List.Extra as List
import Nym exposing (NymTemplate)
import NymDemo.Types exposing (..)
import NymDemo.Vector2 as Vector2 exposing (Vector2)
import NymDemo.Vector3 as Vector3 exposing (Vector3)
import Point exposing (Point)
import Point3d
import Scene3d
import Scene3d.Light
import TupleHelpers
import Utils
import Vector3d
import Viewpoint3d
import WebGL


testView : Model -> Element Msg
testView model =
    case model.morphModels of
        [] ->
            Element.text "derp"

        morphModel :: xs ->
            viewSingleNym
                model.laggedMouse
                morphModel.morphProgress
                morphModel.oldNymTemplate
                morphModel.newNymTempalte


viewSingleNym : Point -> Float -> NymTemplate -> NymTemplate -> Element Msg
viewSingleNym laggedMouse morphProgress oldNymTemplate newNymTemplate =
    Element.el
        [ Element.width <| Element.px 300
        , Element.centerX
        ]
    <|
        viewNymWithPixelDimensions
            ( 300, 300 )
            ( "100%", "100%" )
            laggedMouse
            (interpolateNymsForRendering
                morphProgress
                oldNymTemplate
                newNymTemplate
                |> Nym.renderNymTemplate False
            )
            (Decode.map (always NoOp) (Decode.succeed ()))
            (Decode.map (always NoOp) (Decode.succeed ()))



-- (Decode.map Demos.Morph.MouseMove Mouse.moveDecoder)
-- (Decode.map (always Demos.Morph.NewSeed) (Decode.succeed ()))


viewNymWithPixelDimensions : ( Int, Int ) -> ( String, String ) -> Point -> Scene3d.Entity () -> Decoder msg -> Decoder msg -> Element msg
viewNymWithPixelDimensions renderDimensions displayDimensionStrings lookPoint interpolatedNym onMouseMove onMouseClick =
    let
        ( renderWidth, renderHeight ) =
            renderDimensions

        ( widthStyleStr, heightStyleStr ) =
            displayDimensionStrings
    in
    Element.html <|
        Html.div
            [ Html.Events.on
                "mousemove"
                onMouseMove
            , Html.Events.on
                "mousedown"
                onMouseClick
            ]
        <|
            List.singleton <|
                WebGL.toHtml
                    [ Html.Attributes.style "width" widthStyleStr
                    , Html.Attributes.style "height" heightStyleStr
                    , Html.Attributes.width renderWidth
                    , Html.Attributes.height renderHeight
                    ]
                <|
                    makeWebGLEntities (toFloat renderWidth / toFloat renderHeight)
                        ([ ( interpolatedNym, Point3d.origin ) ]
                            |> rotateNyms (Vector2.fromPoint lookPoint)
                        )


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


makeWebGLEntities : Float -> List (Scene3d.Entity ()) -> List WebGL.Entity
makeWebGLEntities aspectRatio nymList =
    Scene3d.toWebGLEntities
        { lights = Scene3d.noLights
        , camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.origin
                        , eyePoint =
                            Point3d.meters 0 0 7
                        , upDirection = Direction3d.positiveY
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
        , clipDepth = Length.meters 1
        , exposure = Scene3d.exposureValue 5
        , toneMapping = Scene3d.noToneMapping
        , whiteBalance = Scene3d.Light.daylight
        , aspectRatio = aspectRatio
        , supersampling = 1
        , entities = nymList
        }


rotateNyms : Vector2 -> List ( Scene3d.Entity (), Point3dM ) -> List (Scene3d.Entity ())
rotateNyms lookVector entitiesAndPositions =
    entitiesAndPositions
        |> List.map
            (\( nymEntity, position ) ->
                let
                    focusPoint =
                        lookVectorToNymFocusPoint3d lookVector

                    lookDir =
                        Direction3d.from
                            position
                            focusPoint
                            |> Maybe.withDefault Direction3d.z

                    xAngle =
                        Angle.asin <| Direction3d.xComponent lookDir

                    yAngle =
                        Angle.asin <| -(Direction3d.yComponent lookDir)
                in
                nymEntity
                    |> Scene3d.rotateAround
                        Axis3d.y
                        xAngle
                    |> Scene3d.rotateAround
                        (Axis3d.x |> Axis3d.rotateAround Axis3d.y xAngle)
                        yAngle
                    -- |> Scene3d.rotateAround Axis3d.y (Angle.degrees 90)
                    |> Scene3d.translateBy
                        (Vector3d.from Point3d.origin position)
            )


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


lookVectorToNymFocusPoint3d : Vector2 -> Point3dM
lookVectorToNymFocusPoint3d lookVector =
    Point3d.meters
        (lookVector.x * 10.4)
        -(lookVector.y * 10.4)
        2
