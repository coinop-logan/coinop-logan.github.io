module State exposing (..)

import BrickWall.BrickWall as BrickWall exposing (BrickWall)
import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events
import Browser.Navigation as Nav
import Config
import Convert exposing (..)
import NymDemo.State as NymDemo
import NymDemo.Types as NymDemo
import Random
import Responsive exposing (DisplayProfile, viewportToDisplayProfile)
import Route exposing (Route)
import Task
import Time
import Types exposing (..)
import Url exposing (Url)


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Loading
        { viewport = Nothing
        , time_bySecond = Nothing
        , key = key
        , url = url
        }
    , Cmd.batch
        [ Time.now |> Task.perform UpdateNow
        , getViewportCmd
        ]
    )


initLoadedModel : Viewport -> Time.Posix -> Url -> Nav.Key -> ( Model, Cmd Msg )
initLoadedModel viewport now url key =
    ( Loaded
        { key = key
        , route = Route.parseUrl url
        , viewport = viewport
        , bodyViewport = Nothing
        , time_bySecond = now
        , animateTime = now
        , brickWall = Nothing
        , showContactModal = False
        , nymDemoModel = NymDemo.initModel (Random.initialSeed <| Time.posixToMillis now)
        }
    , Cmd.batch
        [ getViewportCmd
        , getBodyViewportCmd
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Loading loadingModel ->
            (case msg of
                GotViewport viewport ->
                    { loadingModel
                        | viewport = Just viewport
                    }

                UpdateNow newNow ->
                    { loadingModel
                        | time_bySecond = Just newNow
                    }

                _ ->
                    loadingModel
            )
                |> (\loadingModelModified ->
                        -- test if loaded
                        case ( loadingModelModified.viewport, loadingModelModified.time_bySecond ) of
                            ( Just viewport, Just time_bySecond ) ->
                                initLoadedModel viewport time_bySecond loadingModelModified.url loadingModelModified.key

                            _ ->
                                ( Loading loadingModelModified
                                , Cmd.none
                                )
                   )

        Loaded loadedModel ->
            updateLoadedModel msg loadedModel
                |> Tuple.mapFirst Loaded


calcNeededPrefill : Viewport -> Float
calcNeededPrefill viewport =
    viewport.viewport.x + viewport.viewport.height


updateLoadedModel : Msg -> LoadedModel -> ( LoadedModel, Cmd Msg )
updateLoadedModel msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotViewport viewport ->
            ( { model
                | viewport = viewport
              }
            , Cmd.none
            )

        GotBodyViewport result ->
            case result of
                Err domErr ->
                    -- let
                    --     _ =
                    --         Debug.log "error getting body viewport" domErr
                    -- in
                    ( model, Cmd.none )

                Ok viewport ->
                    ( { model
                        | bodyViewport = Just viewport
                        , brickWall =
                            Just <|
                                case model.brickWall of
                                    Nothing ->
                                        BrickWall.init viewport model.animateTime

                                    Just brickWall ->
                                        if viewport.scene.width /= brickWall.bodyViewport.scene.width then
                                            BrickWall.init viewport model.animateTime

                                        else
                                            { brickWall
                                                | bodyViewport = viewport
                                            }
                      }
                    , Cmd.none
                    )

        TriggerGetViewports ->
            ( model
            , Cmd.batch
                [ getViewportCmd
                , getBodyViewportCmd
                ]
            )

        GotoRoute route ->
            ( model
            , Cmd.batch
                [ Nav.pushUrl model.key (Route.toString route)
                , Browser.Dom.setViewportOf
                    "body-element"
                    0
                    0
                    |> Task.attempt (always NoOp)
                ]
            )

        OnUrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        OnUrlChange url ->
            ( { model
                | route = Route.parseUrl url
                , bodyViewport = Nothing
              }
            , Cmd.none
            )

        UpdateNow newNow ->
            ( { model | time_bySecond = newNow }
            , Cmd.none
            )

        Animate time ->
            ( { model | animateTime = time }
            , Cmd.none
            )

        AddBricks now ->
            ( case model.brickWall of
                Nothing ->
                    model

                Just brickWall ->
                    { model
                        | brickWall =
                            Just <|
                                let
                                    numToSpawn =
                                        case model.bodyViewport of
                                            Just bodyViewport ->
                                                if bodyViewport.viewport.y > BrickWall.getYOfFirstNothing brickWall then
                                                    15

                                                else
                                                    3

                                            Nothing ->
                                                3
                                in
                                brickWall
                                    |> BrickWall.maybeSpawnNewBricksUnderTargetY numToSpawn now
                    }
            , getBodyViewportCmd
            )

        SetShowContactModal flag ->
            ( { model | showContactModal = flag }
            , Cmd.none
            )

        NymDemoMsg nymDemoMsg ->
            let
                ( newModel, cmd ) =
                    NymDemo.update nymDemoMsg model.nymDemoModel
            in
            ( { model | nymDemoModel = newModel }
            , cmd |> Cmd.map NymDemoMsg
            )

        Test ->
            -- let
            --     _ =
            --         Debug.log "hi " "hi"
            -- in
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 UpdateNow
        , Browser.Events.onAnimationFrame Animate
        , Browser.Events.onResize (\_ _ -> TriggerGetViewports)
        , Time.every 80 AddBricks
        , case model of
            Loaded lModel ->
                Sub.map NymDemoMsg <| NymDemo.subscriptions lModel.nymDemoModel

            _ ->
                Sub.none
        ]


getViewportCmd : Cmd Msg
getViewportCmd =
    Browser.Dom.getViewport |> Task.perform GotViewport


getBodyViewportCmd : Cmd Msg
getBodyViewportCmd =
    Browser.Dom.getViewportOf "body-element" |> Task.attempt GotBodyViewport
