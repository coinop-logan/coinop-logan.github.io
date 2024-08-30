module State exposing (..)

import BrickWall.BrickWall as BrickWall exposing (BrickWall)
import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events
import Browser.Navigation as Nav
import Config
import Convert exposing (..)
import Random
import Responsive exposing (DisplayProfile)
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
        , brickWall = BrickWall.init now 0
        , showContactModal = True
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
                            let
                                oldBW =
                                    model.brickWall
                            in
                            { oldBW
                                | targetY = viewport.viewport.y + viewport.viewport.height
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
            , Nav.pushUrl model.key (Route.toString route)
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
            ( { model | route = Route.parseUrl url }
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
            ( { model
                | brickWall =
                    model.brickWall |> BrickWall.maybeSpawnNewBricksUnderTargetY 3 now
              }
            , getBodyViewportCmd
            )

        -- NameElementSizingInfoGot result ->
        --     case result of
        --         Err _ ->
        --             ( model, Cmd.none )
        --         Ok nameElementSizingInfo ->
        --             ( { model
        --                 | brickWall =
        --                     let
        --                         oldBW =
        --                             model.brickWall
        --                     in
        --                     { oldBW
        --                         | titleArea = Just nameElementSizingInfo.element
        --                     }
        --               }
        --             , Cmd.none
        --             )
        SetShowContactModal flag ->
            ( { model | showContactModal = flag }
            , Cmd.none
            )

        Test ->
            -- let
            --     _ =
            --         Debug.log "hi " "hi"
            -- in
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 UpdateNow
        , Browser.Events.onAnimationFrame Animate
        , Browser.Events.onResize (\_ _ -> TriggerGetViewports)
        , Time.every 15 AddBricks
        ]


getViewportCmd : Cmd Msg
getViewportCmd =
    Browser.Dom.getViewport |> Task.perform GotViewport


getBodyViewportCmd : Cmd Msg
getBodyViewportCmd =
    Browser.Dom.getViewportOf "body-element" |> Task.attempt GotBodyViewport
