module State exposing (..)

import BrickWall.BrickWall as BrickWall exposing (BrickWall)
import Browser.Dom exposing (Viewport)
import Browser.Events
import Config
import Convert exposing (..)
import Random
import Responsive exposing (DisplayProfile)
import Task
import Time
import Types exposing (..)


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( Loading
        { viewport = Nothing
        , time_bySecond = Nothing
        }
    , Cmd.batch
        [ Time.now |> Task.perform UpdateNow
        , getViewportCmd
        ]
    )


initLoadedModel : Viewport -> Time.Posix -> ( Model, Cmd Msg )
initLoadedModel viewport now =
    ( Loaded
        { viewport = viewport
        , time_bySecond = now
        , animateTime = now
        , tabState = OnTab CurrentWork
        , brickWall = BrickWall.init now viewport.scene.height
        }
    , getViewportCmd
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
                                initLoadedModel viewport time_bySecond

                            _ ->
                                ( Loading loadingModelModified
                                , Cmd.none
                                )
                   )

        Loaded loadedModel ->
            updateLoadedModel msg loadedModel
                |> Tuple.mapFirst Loaded


updateLoadedModel : Msg -> LoadedModel -> ( LoadedModel, Cmd Msg )
updateLoadedModel msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotViewport viewport ->
            ( { model
                | viewport = viewport
                , brickWall =
                    let
                        oldBW =
                            model.brickWall
                    in
                    { oldBW
                        | targetY = viewport.viewport.y + viewport.viewport.height
                    }
              }
            , Browser.Dom.getElement "header-element" |> Task.attempt NameElementSizingInfoGot
            )

        TriggerGetViewport ->
            ( model
            , getViewportCmd
            )

        UpdateNow newNow ->
            ( { model | time_bySecond = newNow }
            , Cmd.none
            )

        Animate time ->
            ( { model | animateTime = time }
                |> endAnimationIfNecessary
            , Cmd.none
            )

        CurrentWorkClicked ->
            if Config.animateTabs then
                case model.tabState of
                    OnTab PastWork ->
                        ( { model
                            | tabState =
                                SwitchingTo CurrentWork model.animateTime
                          }
                        , Cmd.none
                        )

                    SwitchingTo CurrentWork _ ->
                        ( model, Cmd.none )

                    OnTab CurrentWork ->
                        ( model, Cmd.none )

                    SwitchingTo PastWork startTime ->
                        ( model |> reverseAnimation
                        , Cmd.none
                        )

            else
                ( { model | tabState = OnTab CurrentWork }
                , Cmd.none
                )

        PortfolioClicked ->
            if Config.animateTabs then
                case model.tabState of
                    OnTab CurrentWork ->
                        ( { model
                            | tabState =
                                SwitchingTo PastWork model.animateTime
                          }
                        , Cmd.none
                        )

                    SwitchingTo PastWork _ ->
                        ( model, Cmd.none )

                    OnTab PastWork ->
                        ( model, Cmd.none )

                    SwitchingTo CurrentWork startTime ->
                        ( model |> reverseAnimation
                        , Cmd.none
                        )

            else
                ( { model | tabState = OnTab PastWork }
                , Cmd.none
                )

        AddBricks now ->
            ( { model
                | brickWall =
                    model.brickWall |> BrickWall.maybeSpawnNewBricksUnderTargetY 3 now
              }
            , getViewportCmd
            )

        NameElementSizingInfoGot result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok nameElementSizingInfo ->
                    ( { model
                        | brickWall =
                            let
                                oldBW =
                                    model.brickWall
                            in
                            { oldBW
                                | titleArea = Just nameElementSizingInfo.element
                            }
                      }
                    , Cmd.none
                    )

        Test ->
            -- let
            --     _ =
            --         Debug.log "hi " "hi"
            -- in
            ( model, Cmd.none )


endAnimationIfNecessary : LoadedModel -> LoadedModel
endAnimationIfNecessary model =
    case model.tabState of
        OnTab _ ->
            model

        SwitchingTo targetTab animateStartTime ->
            if animationProgressFloat animateStartTime model.animateTime >= 1 then
                { model | tabState = OnTab targetTab }

            else
                model


reverseAnimation : LoadedModel -> LoadedModel
reverseAnimation model =
    case model.tabState of
        OnTab _ ->
            model

        SwitchingTo targetTab animateStartTime ->
            let
                targetAnimationProgressFloat =
                    1 - animationProgressFloat animateStartTime model.animateTime

                newStartTime =
                    ((toFloat <| Time.posixToMillis model.animateTime) - (toFloat <| Time.posixToMillis Config.tabSwitchAnimationInterval) * targetAnimationProgressFloat)
                        |> round
                        |> Time.millisToPosix
            in
            { model
                | tabState =
                    SwitchingTo (otherTab targetTab) newStartTime
            }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 UpdateNow
        , Browser.Events.onAnimationFrame Animate
        , Browser.Events.onResize (\_ _ -> TriggerGetViewport)
        , Time.every 15 AddBricks
        ]


getViewportCmd : Cmd Msg
getViewportCmd =
    Browser.Dom.getViewport |> Task.perform GotViewport
