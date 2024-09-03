module NymDemo.State exposing (..)

import Nym exposing (BinarySource, Nym, NymTemplate)
import NymDemo.Config as Config
import NymDemo.Types exposing (..)
import NymSource
import Point exposing (Point)
import Random
import Time


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
    , newNymTempalte = nymTemplate
    , morphProgress = 1
    , morphAccel = 0
    , lastGeneratedSource = binarySource
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
