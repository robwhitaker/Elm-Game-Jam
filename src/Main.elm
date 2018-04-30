module Main exposing (main)

import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Color
import Dict exposing (Dict)
import Task
import Random

import AnimationFrame
import Window

import Game.TwoD.Camera as Camera

import Data.State exposing (..)
import Messages exposing (..)
import ECS exposing (Id)
import ECS.Components.Simple exposing (Position(..))
import ECS.Systems as Systems
import Resource
import KeyboardInput
import Init
import EnemySpawner

main : Program Never State Msg
main =
    Html.program
        { init =
            ( empty
            , Cmd.batch
                [ Task.perform WindowResize Window.size
                , Random.generate (RandomSeed << Random.initialSeed) (Random.int Random.minInt Random.maxInt)
                ]
            ) |> Init.resources
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.batch
            [ AnimationFrame.diffs ((\dt -> dt / 1000) >> Tick)
            , KeyboardInput.subs |> Sub.map KeyboardEvent
            , Window.resizes WindowResize
            ]
        }

update : Msg -> State -> (State, Cmd msg)
update msg state =
    case msg of
        Tick dt ->
            case state.gameState of
                Loading -> (state, Cmd.none)
                _ ->
                    let (newState, cmds) =
                            ECS.runSystems dt state <|
                                (case state.gameState of
                                    Playing -> [ Systems.playerControl ]
                                    WaveTransition _ -> [ Systems.playerControl ]
                                    _ -> [])
                                ++
                                [ Systems.ai
                                , Systems.collision
                                , Systems.physics
                                , Systems.animation
                                ]
                        player = ECS.getEntityBySimpleName "player" newState
                        (Position playerPos) =
                            case player of
                                Nothing -> Position (0,0,0)
                                Just player -> Maybe.withDefault (Position (0,0,0)) player.position
                        cameraPos =
                            \(x, y, _) ->
                                (x*0.8, 250+y*0.2)
                    in
                        if player == Nothing
                            then ({ newState | gameState = GameOver }, cmds )
                        else if EnemySpawner.getTotalRemainingEnemies newState < 1 && state.gameState == Playing
                            then
                                ({ newState
                                    | gameState = WaveTransition 5
                                    , camera = Camera.moveTo (cameraPos playerPos) state.camera
                                    }
                                , cmds
                                )
                        else if state.gameState == Playing
                            then
                                (EnemySpawner.runSpawner dt { newState | camera = Camera.moveTo (cameraPos playerPos) state.camera }, cmds)
                        else
                            case state.gameState of
                                WaveTransition duration ->
                                    if duration - dt > 0
                                        then
                                            ({ newState
                                                | gameState = WaveTransition (duration - dt)
                                                , camera = Camera.moveTo (cameraPos playerPos) state.camera
                                                }
                                            , cmds
                                            )
                                        else
                                            ({ newState
                                                | gameState = Playing
                                                , wave = newState.wave + 1
                                                , enemySpawner = EnemySpawner.newWave (newState.wave + 1) newState.enemySpawner
                                                , camera = Camera.moveTo (cameraPos playerPos) state.camera
                                                }
                                            , cmds
                                            )
                                _ -> ({ newState | camera = Camera.moveTo (cameraPos playerPos) state.camera }, cmds)

        KeyboardEvent e ->
            ({ state | keys = KeyboardInput.update e state.keys }, Cmd.none)

        WindowResize size ->
            ({ state | windowSize = (size.width, size.height) }, Cmd.none)

        LoadTexture loaderMsg ->
            let newLoader = Resource.updateLoader loaderMsg state.resourceLoader
            in
                if newLoader.pending <= 0
                    then (Init.entities { state | resourceLoader = newLoader, gameState = Start }, Cmd.none)
                    else ({ state | resourceLoader = newLoader }, Cmd.none)

        NewGame ->
            ( Init.entities { state | wave = 0, gameState = WaveTransition 5, enemySpawner = EnemySpawner.newWave 1 state.enemySpawner }
            , Cmd.none
            )

        RandomSeed seed ->
            ({ state | enemySpawner = EnemySpawner.newWave state.wave <| EnemySpawner.updateSeed seed state.enemySpawner }
            , Cmd.none
            )

        NoOp -> (state, Cmd.none)

view : State -> Html Msg
view state =
    Html.div
        [ class "game-container" ]
        <| case state.gameState of
            Loading ->
                [ Html.div
                    [ class "loading vcenter" ]
                    [ Html.text
                        ("Loading: "
                        ++ toString (toFloat (state.resourceLoader.total - state.resourceLoader.pending) / toFloat state.resourceLoader.total * 100)
                        ++ "%")
                    ] ]
            _ ->
                [ Systems.render state
                , Html.div
                    [ classList [("wave-info", True), ("hidden", state.gameState == Start)] ]
                    [ Html.div
                        [ class "wave-info-box"]
                        [ Html.h2 [] [ Html.text "Wave:" ]
                        , Html.p [] [ Html.text <| toString state.wave ]
                        ]
                    , Html.div
                        [ class "wave-info-box" ]
                        [ Html.h2 [] [ Html.text "Remaining Enemies:" ]
                        , Html.p [] [ Html.text <| toString (EnemySpawner.getTotalRemainingEnemies state) ]
                        ]
                    ]
                , case state.gameState of
                    Start ->
                        Html.div
                            [ class "center-modal" ]
                            [ Html.h2 [] [ Html.text "Of Stick Figures and Swords" ]
                            , Html.hr [] []
                            , Html.p [ class "info-text" ] [ Html.text "Arrow keys to move. X to attack." ]
                            , Html.button [ onClick NewGame ] [ Html.text "Start" ]
                            ]
                    WaveTransition duration ->
                        Html.div
                            [ class "center-modal" ]
                            [ Html.h2 [] [ Html.text "Next wave starting in:" ]
                            , Html.hr [] []
                            , Html.p [ class "big-text" ] [ Html.text <| toString <| ceiling duration ]
                            ]
                    GameOver ->
                        Html.div
                            [ class "center-modal" ]
                            [ Html.h2 [] [ Html.text "Game Over" ]
                            , Html.hr [] []
                            , Html.p [ class "info-text" ] [ Html.text "Nice." ]
                            , Html.button [ onClick NewGame ] [ Html.text "Try again?" ]
                            ]
                    _ ->
                        Html.span [ class "hidden" ] []
                ]