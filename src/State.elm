module State exposing (..)

import Browser.Dom
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
        , tabState = CurrentWork
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

        UpdateNow newNow ->
            ( { model | time_bySecond = newNow }
            , Cmd.none
            )

        CurrentWorkClicked ->
            ( { model
                | tabState =
                    CurrentWork
              }
            , Cmd.none
            )

        PortfolioClicked ->
            ( { model
                | tabState =
                    Portfolio
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 UpdateNow
