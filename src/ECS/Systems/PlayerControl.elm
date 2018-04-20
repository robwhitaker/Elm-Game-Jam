module ECS.Systems.PlayerControl exposing (playerControl)

import ECS
import ECS.Entity exposing (..)
import ECS.Components.Simple exposing (..)
import ECS.Components.PlayerController exposing (..)
import ECS.Components.Spritesheet exposing (..)
import Data.State exposing (System)
import KeyboardInput exposing (..)

import Vector2 as V2
import Vector3 as V3

playerControl : System msg
playerControl _ =
    ECS.processEntities (\state _ entity ->
        let keys = state.keys
        in
            entity
                |> ECS.with3 .position .physics .playerController (\(Position pos_) (Physics vel mGravity) PlayerController ->
                    { position = pos_, velocity = vel }
                        |> (\p -> -- jumping
                            if V3.getY p.position <= 15 && List.member (Key 38 Pressed) keys
                                then { p | velocity = (V2.getX p.velocity, 1500) }
                                else p)
                        >> (\p -> -- fast falling
                            if V3.getY p.position > 15 && List.member (Key 40 Pressed) keys
                                then { p | velocity = (V2.getX p.velocity, -1500) }
                                else p
                            )
                        >> (\p -> -- horizontal movement
                            case List.head (List.filter (\(Key n _) -> n == 37 || n == 39) keys) of
                                Nothing -> { p | velocity = (0, V2.getY p.velocity) }
                                Just (Key n _) ->
                                    case n of
                                        37 -> { p | velocity = (-750, V2.getY p.velocity) }
                                        39 -> { p | velocity = (750, V2.getY p.velocity) }
                                        _ -> p
                            )
                        >> (\p ->
                            entity
                                |> ECS.set position_ (Position p.position)
                                |> ECS.set physics_ (Physics p.velocity mGravity)
                                |> ECS.update spritesheet_ (\maybeSpritesheet ->
                                    case maybeSpritesheet of
                                        Nothing -> Nothing
                                        Just spritesheet ->
                                            if keyDown 88 keys
                                                then
                                                    case getRunningAnimation spritesheet of
                                                        Nothing -> Just spritesheet
                                                        Just anim ->
                                                            let width = (V2.getX anim.currentAnimation.size)
                                                                dir = width / (abs width)
                                                            in
                                                                loadRunningAnimation "attack" spritesheet
                                                                    |> mapCurrentAnimation (\cAnim ->
                                                                        { cAnim
                                                                            | size = (,)
                                                                                ((abs (V2.getX cAnim.size)) * dir)
                                                                                (V2.getY cAnim.size)
                                                                            , pivot =
                                                                                if dir < 0 then
                                                                                    (0.35,0)
                                                                                else
                                                                                    (0,0)
                                                                        }) |> Just
                                                else
                                                    if V2.getX p.velocity == 0
                                                        then
                                                            case getRunningAnimation spritesheet of
                                                                Nothing -> Just (loadRunningAnimation "idle" spritesheet)
                                                                Just anim ->
                                                                    let width = (V2.getX anim.currentAnimation.size)
                                                                        dir = width / (abs width)
                                                                    in
                                                                        loadRunningAnimation "idle" spritesheet
                                                                            |> mapCurrentAnimation (\cAnim ->
                                                                                { cAnim
                                                                                    | size = (,)
                                                                                        ((abs (V2.getX cAnim.size)) * dir)
                                                                                        (V2.getY cAnim.size)
                                                                                    , pivot =
                                                                                        if dir < 0 then
                                                                                            (0.5,0)
                                                                                        else
                                                                                            (0,0)
                                                                                }) |> Just
                                                        else
                                                            let velX = V2.getX p.velocity
                                                                dir = velX / (abs velX)
                                                                spritesheetNew = loadRunningAnimation "running" spritesheet
                                                            in
                                                                case getRunningAnimation spritesheetNew of
                                                                    Nothing -> Just spritesheetNew
                                                                    Just anim ->
                                                                        let cAnim = anim.currentAnimation
                                                                        in
                                                                            setRunningAnimation
                                                                                (Just { anim
                                                                                    | currentAnimation =
                                                                                        { cAnim
                                                                                            | size = (,)
                                                                                                ((abs (V2.getX cAnim.size)) * dir)
                                                                                                (V2.getY cAnim.size)
                                                                                            , pivot =
                                                                                                if dir < 1 then
                                                                                                    (0.5,0)
                                                                                                else
                                                                                                    (0,0)
                                                                                        }
                                                                                }) spritesheetNew |> Just
                                )
                        )
                ) |> (\e -> (state, e, Cmd.none))
    ) >> (\(s, c) ->
        ({ s | keys = KeyboardInput.setHoldingAll s.keys }, c) )
