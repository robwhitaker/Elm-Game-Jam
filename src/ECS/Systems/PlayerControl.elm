module ECS.Systems.PlayerControl exposing (playerControl)

import ECS
import ECS.Entity exposing (..)
import ECS.Components.Simple exposing (..)
import ECS.Components.PlayerController exposing (..)
import ECS.Components.Spritesheet exposing (..)
import Data.State exposing (System, baseHeight)
import KeyboardInput exposing (..)

import Dict
import Vector2 as V2
import Vector3 as V3

playerControl : System msg
playerControl dt =
    ECS.processEntities (\state _ entity ->
        let keys = state.keys
        in
            entity
                |> ECS.with .position
                |> ECS.andWith .physics
                |> ECS.andWith .direction
                |> ECS.andWith .speed
                |> ECS.andWith .spritesheet
                |> ECS.andWith .playerController
                |> ECS.processEntity
                    (\(Position pos_) (Physics vel mGravity) direction (Speed moveSpeed) (Spritesheet _ runningAnimation animations as spritesheet) (PlayerController playerState) ->
                        { position = pos_, velocity = vel, playerState = playerState, direction = direction }
                            |> (\p -> -- clear/update controls from last inputs
                                case p.playerState of
                                    Attacking duration ->
                                        if duration - dt <= 0
                                            then { p | playerState = Idle }
                                            else { p | playerState = Attacking (duration - dt) }
                                    _ -> p
                                )
                            |> (\p -> -- jumping
                                if V3.getY p.position <= baseHeight && List.member (Key 38 Pressed) keys
                                    then { p | velocity = (V2.getX p.velocity, 2000), playerState = if isAttacking p.playerState then Attacking 0 else p.playerState }
                                    else p
                                )
                            >> (\p -> -- fast falling
                                if V3.getY p.position > baseHeight && V2.getY p.velocity <= 1250 && List.member (Key 40 Pressed) keys
                                    then { p | velocity = (V2.getX p.velocity, min (V2.getY p.velocity - 500) -2500) }
                                    else p
                                )
                            >> (\p -> -- attacking
                                if List.member (Key 88 Pressed) keys
                                    then
                                        let attackDuration =
                                                Dict.get "attack" animations
                                                    |> Maybe.andThen (Just << .duration)
                                                    |> Maybe.withDefault 0
                                        in
                                            { p | playerState = Attacking attackDuration }
                                    else p
                                )
                            >> (\p -> -- horizontal movement
                                let border = 3100
                                    maybeInputDir =
                                        case List.head (List.filter (\(Key n _) -> n == 37 || n == 39) keys) of
                                            Just (Key 37 _) ->
                                                if V3.getX p.position <= -border then Nothing else Just Left
                                            Just (Key 39 _) ->
                                                if V3.getX p.position >= border then Nothing else Just Right
                                            _               -> Nothing
                                    skidSpeed x = (V2.getX p.velocity)/x
                                    skidToHalt x =
                                        if abs (skidSpeed x) < 10
                                            then 0
                                            else skidSpeed x
                                in
                                    case maybeInputDir of
                                        Nothing ->
                                            { p | velocity =
                                                ( if V3.getY p.position <= baseHeight
                                                    then skidToHalt 1.3
                                                    else skidToHalt 1.1
                                                , V2.getY p.velocity
                                                )
                                            }
                                        Just inputDir ->
                                            let facing =
                                                    if V3.getY p.position > baseHeight || isAttacking p.playerState
                                                        then p.direction
                                                        else inputDir
                                                newVelX =
                                                    if isAttacking p.playerState && V3.getY p.position <= baseHeight
                                                        then skidToHalt 1.1
                                                        else turn inputDir moveSpeed
                                                newPState =
                                                    if newVelX /= 0 && not (isAttacking p.playerState)
                                                        then Running
                                                        else p.playerState
                                            in
                                                { p | velocity = (newVelX, V2.getY p.velocity)
                                                    , direction = facing
                                                    , playerState = newPState
                                                }
                                )
                            >> (\p ->
                                if abs (V2.getX p.velocity) < moveSpeed && not (isAttacking p.playerState)
                                    then { p | playerState = Idle }
                                    else p
                                )
                            >> (\p ->
                                entity
                                    |> ECS.set playerController_ (PlayerController p.playerState)
                                    |> ECS.set position_ (Position p.position)
                                    |> ECS.set physics_ (Physics p.velocity mGravity)
                                    |> ECS.set direction_ p.direction
                                    |> ECS.set spritesheet_
                                        ( let newAnim =
                                            case p.playerState of
                                                Attacking _ -> "attack"
                                                Idle -> "idle"
                                                Running -> "running"
                                        in
                                            loadRunningAnimation newAnim spritesheet )
                                )
                    ) |> (\e -> (state, e, Cmd.none))
        ) >> (\(s, c) ->
            ({ s | keys = KeyboardInput.setHoldingAll s.keys }, c) )
