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
import NymDemo.Common exposing (..)
import NymDemo.Config as Config
import NymDemo.Types exposing (..)
import NymDemo.Vector2 as Vector2 exposing (Vector2)
import NymDemo.Vector3 as Vector3 exposing (Vector3)
import Point exposing (Point)
import Point3d
import Responsive exposing (DisplayProfile(..), responsiveVal)
import Scene3d
import Scene3d.Light
import TupleHelpers
import Utils
import Vector3d
import Viewpoint3d
import WebGL


view : DisplayProfile -> Model -> Element Msg
view dProfile model =
    viewMorphingNyms
        dProfile
        model.laggedMouse
        model.morphModels


viewMorphingNyms : DisplayProfile -> Point -> List MorphModel -> Element Msg
viewMorphingNyms dProfile lookPoint morphModels =
    let
        interpolatedNyms =
            morphModels
                |> List.map renderMorphingModel
    in
    viewNyms
        dProfile
        lookPoint
        interpolatedNyms


renderMorphingModel : MorphModel -> Scene3d.Entity ()
renderMorphingModel morphModel =
    interpolateNymsForRendering
        morphModel.morphProgress
        morphModel.oldNymTemplate
        morphModel.newNymTemplate
        |> Nym.renderNymTemplate False


viewNyms : DisplayProfile -> Point -> List (Scene3d.Entity ()) -> Element Msg
viewNyms dProfile lookPoint interpolatedNyms =
    let
        ( renderWidth, renderHeight ) =
            Config.nymDemoRenderDimensions dProfile

        nymPositions =
            responsiveVal dProfile
                [ Point3d.meters -1 -1 0
                , Point3d.meters 1 -1 0
                , Point3d.meters -1 1 0
                , Point3d.meters 1 1 0
                ]
                [ Point3d.meters -4.5 0 0
                , Point3d.meters -1.5 0 0
                , Point3d.meters 1.5 0 0
                , Point3d.meters 4.5 0 0
                ]

        nymsAndPositions =
            List.map2
                Tuple.pair
                interpolatedNyms
                nymPositions
    in
    Element.html <|
        WebGL.toHtml
            [ Html.Attributes.style "width" "100%"
            , Html.Attributes.style "height" "100%"
            , Html.Attributes.width renderWidth
            , Html.Attributes.height renderHeight
            , Html.Events.on "mousemove" (Decode.map MouseMove mouseMoveDecoder)

            -- , Html.Events.on "mousedown" (Decode.map (always Demos.Morph.NewSeed) (Decode.succeed ()))
            ]
        <|
            makeWebGLEntities dProfile
                (toFloat renderWidth / toFloat renderHeight)
                (nymsAndPositions
                    |> rotateNyms (Vector2.fromPoint lookPoint)
                )


makeWebGLEntities : DisplayProfile -> Float -> List (Scene3d.Entity ()) -> List WebGL.Entity
makeWebGLEntities dProfile aspectRatio nymList =
    Scene3d.toWebGLEntities
        { lights = Scene3d.noLights
        , camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.origin
                        , eyePoint = Point3d.meters 0 0 (Config.cameraHeight dProfile)
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


lookVectorToNymFocusPoint3d : Vector2 -> Point3dM
lookVectorToNymFocusPoint3d lookVector =
    Point3d.meters
        (lookVector.x * 10.4)
        -(lookVector.y * 10.4)
        2


mouseMoveDecoder : Decoder MouseMoveData
mouseMoveDecoder =
    Decode.map4 MouseMoveData
        (Decode.at [ "offsetX" ] Decode.int)
        (Decode.at [ "offsetY" ] Decode.int)
        (Decode.at [ "target", "offsetHeight" ] Decode.float)
        (Decode.at [ "target", "offsetWidth" ] Decode.float)
