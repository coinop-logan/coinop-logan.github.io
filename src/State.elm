module State exposing (..)

import Bricks.State as Bricks
import Browser.Dom
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
        { dProfile = Nothing
        , time_bySecond = Nothing
        }
    , Cmd.batch
        [ Time.now |> Task.perform UpdateNow
        , Browser.Dom.getViewport |> Task.perform GotViewport
        ]
    )


initLoadedModel : DisplayProfile -> Time.Posix -> ( Model, Cmd Msg )
initLoadedModel dProfile now =
    ( Loaded
        { dProfile = dProfile
        , time_bySecond = now
        , animateTime = now
        , tabState = OnTab CurrentWork
        , bricksModel = Bricks.init (Time.posixToMillis now) 10
        }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Loading loadingModel ->
            (case msg of
                GotViewport viewport ->
                    { loadingModel
                        | dProfile =
                            Just <| Responsive.screenWidthToDisplayProfile <| floor viewport.viewport.width
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
                        case ( loadingModelModified.dProfile, loadingModelModified.time_bySecond ) of
                            ( Just dProfile, Just time_bySecond ) ->
                                initLoadedModel dProfile time_bySecond

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
                | dProfile =
                    Responsive.screenWidthToDisplayProfile <| floor viewport.viewport.width
              }
            , Cmd.none
            )

        Resize width _ ->
            ( { model
                | dProfile =
                    Responsive.screenWidthToDisplayProfile width
              }
            , Cmd.none
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
                | bricksModel =
                    let
                        oldBricksModel =
                            model.bricksModel
                    in
                    { oldBricksModel
                        | bricks =
                            oldBricksModel.bricks
                                |> Bricks.addNewBrick oldBricksModel.seedSeed now
                    }
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
        , Browser.Events.onResize Resize
        , Time.every 30 TestBrickShit
        ]
