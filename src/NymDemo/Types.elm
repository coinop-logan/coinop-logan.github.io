module NymDemo.Types exposing (..)

import Length
import Nym exposing (BinarySource, GenError, NymTemplate)
import Point exposing (Point)
import Point3d
import Random
import Time


type alias Model =
    { mouseInput : Point
    , laggedMouse : Point
    , morphModels : List MorphModel
    , lastMouseMoveTime : Time.Posix
    , lastMouseClickTime : Time.Posix
    , now : Time.Posix
    , seed : Random.Seed
    }


type Msg
    = NoOp


type alias MorphModel =
    { oldNymTemplate : NymTemplate
    , newNymTempalte : NymTemplate
    , morphProgress : Float
    , morphAccel : Float
    , lastGeneratedSource : BinarySource
    }


type alias Point3dM =
    Point3d.Point3d Length.Meters ()
