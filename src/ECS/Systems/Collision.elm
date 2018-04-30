module ECS.Systems.Collision exposing (collision)

import Collision2D

import Dict

import ECS
import ECS.Entity exposing (..)
import ECS.Components.Simple exposing (..)
import ECS.Components.Spritesheet exposing (..)
import ECS.Components.Collision exposing (..)
import ECS.Components.AudioPlayer exposing (..)
import Data.State exposing (System)
import Utils.SelectionList as SL

collision : System msg
collision dt =
    ECS.processEntities (\state id entity ->
        entity
            |> ECS.with .collision
            |> ECS.andWith .position
            |> ECS.andWith .spritesheet
            |> ECS.processEntity (\(Collision collisionClass) (Position pos) (Spritesheet _ maybeRunningAnimation _) ->
                ECS.getEntityIdBySimpleName "player" state
                |> Maybe.andThen (\playerId -> ECS.getEntityById playerId state
                    |> Maybe.andThen (\player ->
                        player
                            |> ECS.with .collision
                            |> ECS.andWith .position
                            |> ECS.andWith .spritesheet
                            |> ECS.processEntity (\_ (Position playerPos) (Spritesheet _ playerRunningAnimation _) ->
                                case collisionClass of
                                    Player -> (state, Nothing, Cmd.none)
                                    Enemy ->
                                        playerRunningAnimation
                                            |> Maybe.andThen (\playerRA ->
                                        playerRA.currentAnimation.hitboxes
                                            |> Maybe.andThen (\playerHitboxes ->
                                        maybeRunningAnimation
                                            |> Maybe.andThen (\enemyRA ->
                                        enemyRA.currentAnimation.hitboxes
                                            |> Maybe.andThen (\enemyHitboxes ->
                                        let pBoxes = SL.selected playerHitboxes
                                            eBoxes = SL.selected enemyHitboxes
                                            pCollisionRect = .collisionRect << worldRect playerPos playerRA.currentAnimation.size playerRA.currentAnimation.pivot
                                            eCollisionRect = .collisionRect << worldRect pos enemyRA.currentAnimation.size enemyRA.currentAnimation.pivot
                                            -- O(n^2)? Bleh
                                            (newPlayer, newEntity) = List.foldl (\phbox (newPlayer, newEntity) ->
                                                List.foldl (\ehbox ((np, ne) as acc) ->
                                                    case (getHitboxType phbox, getHitboxType ehbox) of --only care about a hit and hurt box colliding
                                                        (Hurtbox _, Hurtbox _) -> acc
                                                        (Damagebox _, Damagebox _) -> acc
                                                        _ ->
                                                            let pcRect = pCollisionRect (getRect phbox)
                                                                ecRect = eCollisionRect (getRect ehbox)
                                                            in
                                                                if Collision2D.axisAlignedBoundingBox pcRect ecRect
                                                                    then
                                                                        let playHurtSound =
                                                                                ECS.update audioPlayer_ (Maybe.map (queueAudio "gothit"))
                                                                            hpUpdater dmg mult maybeE =
                                                                                case maybeE of
                                                                                    Nothing -> Nothing
                                                                                    Just e ->
                                                                                        if Maybe.map (\(HP hp _) -> hp - (dmg * mult) <= 0) e.hp == Just True
                                                                                            then Nothing
                                                                                            else
                                                                                                Just
                                                                                                    (ECS.update hp_
                                                                                                        (Maybe.map (\(HP hp maxHP) -> HP (hp - (dmg * mult)) maxHP))
                                                                                                        e
                                                                                                        )
                                                                        in
                                                                            case (getHitboxType phbox, getHitboxType ehbox) of
                                                                                (Damagebox dmg, Hurtbox mult) ->
                                                                                    ( np
                                                                                    , hpUpdater dmg mult ne |> Maybe.map playHurtSound
                                                                                    )
                                                                                (Hurtbox mult, Damagebox dmg) ->
                                                                                    ( hpUpdater dmg mult np |> Maybe.map playHurtSound
                                                                                    , ne
                                                                                    )
                                                                                _ -> acc
                                                                    else acc
                                                ) (newPlayer, newEntity) eBoxes
                                            ) (Just player, Just entity) pBoxes
                                            updateState i e s =
                                                case e of
                                                    Nothing -> ECS.removeEntityById i s
                                                    Just newE -> ECS.updateEntityById i (always newE) s
                                        in
                                            Just
                                                ( state |> updateState playerId newPlayer |> updateState id newEntity
                                                , Nothing
                                                , Cmd.none
                                                )
                                        )))) |> Maybe.withDefault (state, Nothing, Cmd.none)
                        )
                    )
                )
            ) |> Maybe.andThen identity |> Maybe.withDefault (state, Nothing, Cmd.none)
    )