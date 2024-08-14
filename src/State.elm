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
        , Browser.Dom.getViewport |> Task.perform GotViewport
        ]
    )


initLoadedModel : Viewport -> Time.Posix -> ( Model, Cmd Msg )
initLoadedModel viewport now =
    ( Loaded
        { viewport = viewport
        , time_bySecond = now
        , animateTime = now
        , tabState = OnTab CurrentWork
        , brickWall = BrickWall.init now 1
        }
    , Browser.Dom.getViewport |> Task.perform GotViewport
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
              }
            , Cmd.none
            )

        TriggerGetViewport ->
            ( model
            , Browser.Dom.getViewport
                |> Task.perform GotViewport
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
            case model.tabState of
                OnTab Portfolio ->
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

                SwitchingTo Portfolio startTime ->
                    ( model |> reverseAnimation
                    , Cmd.none
                    )

        PortfolioClicked ->
            case model.tabState of
                OnTab CurrentWork ->
                    ( { model
                        | tabState =
                            SwitchingTo Portfolio model.animateTime
                      }
                    , Cmd.none
                    )

                SwitchingTo Portfolio _ ->
                    ( model, Cmd.none )

                OnTab Portfolio ->
                    ( model, Cmd.none )

                SwitchingTo CurrentWork startTime ->
                    ( model |> reverseAnimation
                    , Cmd.none
                    )

        TestBrickShit now ->
            ( { model
                | brickWall =
                    model.brickWall
                        |> BrickWall.updateBrickStates now
                        |> BrickWall.spawnNewBrick now
              }
            , Cmd.none
            )


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
        , Time.every 15 TestBrickShit
        ]
