module Main exposing (main)

import Html exposing (Html)
import Html.Attributes exposing (..)
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
            let (newState, cmds) =
                    ECS.runSystems dt state
                        [ Systems.playerControl
                        , Systems.ai
                        , Systems.collision
                        , Systems.physics
                        , Systems.animation
                        ]
                newEntities = newState.entities
                (Position playerPos) =
                    case Dict.get 0 newEntities of
                        Nothing -> Position (0,0,0)
                        Just player -> Maybe.withDefault (Position (0,0,0)) player.position
                cameraPos =
                    \(x, y, _) ->
                        (x*0.8, 300+y*0.2)
            in
                (EnemySpawner.runSpawner dt { newState | camera = Camera.moveTo (cameraPos playerPos) state.camera }, cmds)

        KeyboardEvent e ->
            ({ state | keys = KeyboardInput.update e state.keys }, Cmd.none)

        WindowResize size ->
            ({ state | windowSize = (size.width, size.height) }, Cmd.none)

        LoadTexture loaderMsg ->
            let newLoader = Resource.updateLoader loaderMsg state.resourceLoader
                a = Debug.log "Loads pending" (toString newLoader.pending)
            in
                if newLoader.pending <= 0
                    then (Init.entities { state | resourceLoader = newLoader }, Cmd.none)
                    else ({ state | resourceLoader = newLoader }, Cmd.none)

        RandomSeed seed ->
            ({ state | enemySpawner = EnemySpawner.newWave state.wave <| EnemySpawner.updateSeed seed state.enemySpawner }
            , Cmd.none
            )

        NoOp -> (state, Cmd.none)

view : State -> Html msg
view state =
    Html.div
        [ style [("width", "100vw"), ("height","100vh"), ("position", "relative")] ]
        [ Systems.render state
        , Html.div
            [ style [("position", "absolute"), ("top", "0"), ("left", "0")] ]
            [ Html.p [] [ Html.text <| "Wave: " ++ toString state.wave ]
            , Html.p [] [ Html.text <| "Enemies remaining: " ++ toString (EnemySpawner.getTotalRemainingEnemies state) ]
            ]
        ]

