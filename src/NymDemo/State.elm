module NymDemo.State exposing (..)

import List.Extra as List
import Nym exposing (BinarySource, Nym, NymTemplate)
import NymDemo.Common exposing (..)
import NymDemo.Config as Config
import NymDemo.Types exposing (..)
import NymSource
import Point exposing (Point)
import Random
import Time
import TupleHelpers
import Vector2 exposing (Vector2)


initModel : Random.Seed -> Model
initModel seed =
    let
        ( morphModels, newSeed ) =
            Random.step
                (Random.list Config.numNyms
                    (NymSource.binarySourceGenerator
                        |> Random.map initMorphModel
                    )
                )
                seed
    in
    { mouseInput = Point 0 0
    , laggedMouse = Point 0 0
    , morphModels = morphModels
    , lastMouseMoveTime = Time.millisToPosix 0
    , lastMouseClickTime = Time.millisToPosix 0
    , now = Time.millisToPosix 0
    , seed = newSeed
    }


initMorphModel : BinarySource -> MorphModel
initMorphModel binarySource =
    let
        nymTemplate =
            Nym.binarySourceToNymTemplate binarySource
                |> (\( _, _, c ) -> c)
    in
    { oldNymTemplate = nymTemplate
    , newNymTemplate = nymTemplate
    , morphProgress = 1
    , morphAccel = 0
    , lastGeneratedSource = binarySource
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        MouseMove moveData ->
            ( { model
                | mouseInput =
                    mouseMoveDataToLookDir moveData
                , lastMouseMoveTime = model.now
              }
            , Cmd.none
            )

        NewSeed i ->
            ( { model
                | lastMouseClickTime = model.now
              }
                |> updateMorphModelWithNewSource i
            , Cmd.none
            )

        AnimateDelta delta ->
            let
                mouseInterpConstant =
                    if mouseMoveIsIdle model then
                        0.01

                    else
                        0.1

                morphRateConstant =
                    if mouseClickIsIdle model then
                        5000

                    else
                        5000
            in
            ( { model
                | morphModels =
                    model.morphModels
                        |> List.map
                            (\morphModel ->
                                let
                                    morphAccel =
                                        morphModel.morphAccel + (delta / morphRateConstant)
                                in
                                { morphModel
                                    | morphAccel = morphAccel
                                    , morphProgress =
                                        min
                                            1
                                            (morphModel.morphProgress + morphAccel)
                                }
                            )
                , laggedMouse =
                    Vector2.interpolate mouseInterpConstant model.laggedMouse model.mouseInput
              }
            , Cmd.none
            )

        MaybeChangeLookDir ->
            let
                ( newSeed, morphModels ) =
                    model.morphModels
                        |> List.mapAccuml
                            (\seed morphModel ->
                                Random.step
                                    (Random.andThen
                                        (\newBSource ->
                                            Random.uniform morphModel.lastGeneratedSource [ newBSource ]
                                                |> Random.map (\bs -> setMorphModelSource bs morphModel)
                                        )
                                        NymSource.binarySourceGenerator
                                    )
                                    seed
                                    |> TupleHelpers.swap
                            )
                            model.seed
            in
            ( { model
                | morphModels = morphModels
                , seed = newSeed
              }
            , Cmd.none
            )

        MaybeChangeSeed _ ->
            ( model, Cmd.none )



-- let
--     maybeNewInputAndSource =
--         if not <| mouseMoveIsIdle model then
--             Nothing
--         else
--             model.lastGeneratedSource
--                 |> BinarySource.cycleWithSalt (time |> Time.posixToMillis |> String.fromInt)
--                 |> BinarySource.consume2
--                     ( BinarySource.consumeBool
--                     , BinarySource.consume2
--                         ( BinarySource.consumeFloatRange 3 ( -0.3, 0.3 )
--                         , BinarySource.consumeFloatRange 3 ( -0.2, 0.4 )
--                         )
--                     )
--                 |> Maybe.andThen
--                     (\( newSource, ( shouldChange, newDir ), _ ) ->
--                         if shouldChange then
--                             Nothing
--                         else
--                             Just <| ( newSource, Vector2.fromTuple newDir )
--                     )
--     newModel =
--         case maybeNewInputAndSource of
--             Just ( newSource, newInput ) ->
--                 { model
--                     | mouseInput = newInput
--                     , lastGeneratedSource = newSource
--                 }
--             Nothing ->
--                 model
-- in
-- ( newModel
-- , Cmd.none
-- )


mouseMoveDataToLookDir : MouseMoveData -> Vector2
mouseMoveDataToLookDir moveData =
    Vector2
        (toFloat moveData.offsetX / moveData.offsetWidth - 0.5)
        (toFloat moveData.offsetY / moveData.offsetHeight - 0.5)


updateMorphModelWithNewSource : Int -> Model -> Model
updateMorphModelWithNewSource i model =
    let
        ( newSource, newSeed ) =
            Random.step
                NymSource.binarySourceGenerator
                model.seed
    in
    { model
        | morphModels =
            model.morphModels
                |> List.updateAt i (\morphModel -> setMorphModelSource newSource morphModel)
        , seed = newSeed
    }


setMorphModelSource : BinarySource -> MorphModel -> MorphModel
setMorphModelSource binarySource morphModel =
    { morphModel
        | lastGeneratedSource = binarySource
        , oldNymTemplate =
            interpolateNymsForRendering
                morphModel.morphProgress
                morphModel.oldNymTemplate
                morphModel.newNymTemplate
        , newNymTemplate = genNymTemplate binarySource
        , morphProgress = 0
        , morphAccel = 0
    }


mouseMoveIsIdle : Model -> Bool
mouseMoveIsIdle model =
    Time.toSecond Time.utc model.now - Time.toSecond Time.utc model.lastMouseMoveTime > 2


mouseClickIsIdle : Model -> Bool
mouseClickIsIdle model =
    Time.toSecond Time.utc model.now - Time.toSecond Time.utc model.lastMouseClickTime > 4
