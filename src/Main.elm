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
update msg model =
    case msg of
        Tick dt ->
            let (newState, cmds) =
                    ECS.runSystems dt model
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
                ({model
                    | entities = newEntities
                    , camera = Camera.moveTo (cameraPos playerPos) model.camera
                    }
                , cmds)

        KeyboardEvent e ->
            ({ model | keys = KeyboardInput.update e model.keys }, Cmd.none)

        WindowResize size ->
            ({ model | windowSize = (size.width, size.height) }, Cmd.none)

        LoadTexture loaderMsg ->
            let newLoader = Resource.updateLoader loaderMsg model.resourceLoader
                a = Debug.log "Loads pending" (toString newLoader.pending)
            in
                if newLoader.pending <= 0
                    then (Init.entities { model | resourceLoader = newLoader }, Cmd.none)
                    else ({ model | resourceLoader = newLoader }, Cmd.none)

        NoOp -> (model, Cmd.none)

view : State -> Html msg
view = Systems.render
