module Main exposing (main)

import Html exposing (Html)
import Color
import Dict exposing (Dict)
import Task

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

main : Program Never State Msg
main =
    Html.program
        { init =
            ( empty
            , Cmd.batch
                [ Task.perform WindowResize Window.size ]
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
                        , Systems.physics
                        , Systems.animation
                        ]
                newEntities = newState.entities
                --TODO: this should be handled via a system
                (Position playerPos) =
                    case Dict.get 0 newEntities of
                        Nothing -> Position (0,0,0)
                        Just player -> Maybe.withDefault (Position (0,0,0)) player.position
                cameraPos =
                    \(x, y, _) ->
                        (x*0.8, 300+y*0.2)
            in
                ({ newState | camera = Camera.moveTo (cameraPos playerPos) state.camera }, cmds)

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

        NoOp -> (state, Cmd.none)

view : State -> Html msg
view = Systems.render
