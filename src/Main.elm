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
                Playing ->
                    let (newState, cmds) =
                            ECS.runSystems dt state
                                [ Systems.playerControl
                                , Systems.ai
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
                                (x*0.8, 300+y*0.2)
                    in
                        if player == Nothing
                            then ({ newState | gameState = GameOver }, cmds )
                        else if EnemySpawner.getTotalRemainingEnemies newState < 1
                            then
                                ({ newState
                                    | gameState = WaveTransition 5
                                    , wave = newState.wave + 1
                                    , enemySpawner = EnemySpawner.newWave (newState.wave + 1) newState.enemySpawner
                                    }
                                , cmds
                                )
                        else
                            (EnemySpawner.runSpawner dt { newState | camera = Camera.moveTo (cameraPos playerPos) state.camera }, cmds)
                WaveTransition duration ->
                    ( if duration - dt > 0
                        then { state | gameState = WaveTransition (duration - dt) }
                        else { state | gameState = Playing}
                    , Cmd.none
                    )
                _ ->
                    (state, Cmd.none)

        KeyboardEvent e ->
            ({ state | keys = KeyboardInput.update e state.keys }, Cmd.none)

        WindowResize size ->
            ({ state | windowSize = (size.width, size.height) }, Cmd.none)

        LoadTexture loaderMsg ->
            let newLoader = Resource.updateLoader loaderMsg state.resourceLoader
            in
                if newLoader.pending <= 0
                    then ({ state | resourceLoader = newLoader, gameState = Start }, Cmd.none)
                    else ({ state | resourceLoader = newLoader }, Cmd.none)

        NewGame ->
            ( Init.entities { state | wave = 1, gameState = WaveTransition 5, enemySpawner = EnemySpawner.newWave 1 state.enemySpawner }
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
        [ style [("width", "100vw"), ("height","100vh"), ("position", "relative")] ]
        <| case state.gameState of
            Loading -> [ Html.text ("Loading: " ++ toString (toFloat (state.resourceLoader.total - state.resourceLoader.pending) / toFloat state.resourceLoader.total * 100) ++ "%") ]
            Start ->
                [ Html.button [ onClick NewGame ] [ Html.text "Play" ] ]
            GameOver ->
                [ Html.text "Game Over"
                , Html.button [ onClick NewGame ] [ Html.text "Try Again?" ]
                ]
            _ ->
                [ Systems.render state
                , Html.div
                    [ style [("position", "absolute"), ("top", "0"), ("left", "0")] ]
                    [ Html.p [] [ Html.text <| "Wave: " ++ toString state.wave ]
                    , Html.p [] [ Html.text <| "Enemies remaining: " ++ toString (EnemySpawner.getTotalRemainingEnemies state) ]
                    , case state.gameState of
                        WaveTransition duration ->
                            Html.p [] [ Html.text <| "Next wave in: " ++ toString duration ]
                        _ -> Html.text ""
                    ]
                ]

