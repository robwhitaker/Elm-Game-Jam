module ECS.Systems.Collision exposing (collision)

import Collision2D

import Dict

import ECS
import ECS.Entity exposing (..)
import ECS.Components.Simple exposing (..)
import ECS.Components.Spritesheet exposing (..)
import ECS.Components.Collision exposing (..)
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
                ECS.getEntityBySimpleName "player" state
                    |> Maybe.andThen
                        ( ECS.with .collision
                        >> ECS.andWith .position
                        >> ECS.andWith .spritesheet
                        >> ECS.processEntity (\_ (Position playerPos) (Spritesheet _ playerRunningAnimation _) ->
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
                                        (playerGotHit, enemyGotHit) = List.foldl (\phbox (playerGotHit, enemyGotHit) ->
                                            let (phit, ehit) =
                                                    List.foldl (\ehbox (ph,eh) ->
                                                        -- let a = Debug.log "boxes" (toString (phbox, ehbox))
                                                        -- in
                                                        case (phbox, ehbox) of --only care about a hit and hurt box colliding
                                                            (Hurtbox _, Hurtbox _) -> (ph,eh)
                                                            (Hitbox _, Hitbox _) -> (ph,eh)
                                                            _ ->
                                                                let pcRect = pCollisionRect (getRect phbox)
                                                                    ecRect = eCollisionRect (getRect ehbox)
                                                                    a = Debug.log "wat" (toString (pcRect, ecRect))
                                                                in
                                                                    if Collision2D.axisAlignedBoundingBox pcRect ecRect
                                                                        then
                                                                            case phbox of
                                                                                (Hurtbox _) -> (True,eh)
                                                                                (Hitbox _) -> (ph,True)
                                                                        else (ph, eh)
                                                    ) (False, False) eBoxes
                                            in
                                                (playerGotHit || phit, enemyGotHit || ehit)
                                        ) (False, False) pBoxes
                                        -- a = Debug.log "Um" (toString (playerGotHit, enemyGotHit))
                                    in
                                        Just
                                            ( if playerGotHit then ECS.updateEntityBySimpleName "player" processHit state else state
                                            , if enemyGotHit then Just (processHit entity) else Nothing
                                            , Cmd.none
                                            )
                                    )))) |> Maybe.withDefault (state, Nothing, Cmd.none)
                        )
                    )
            ) |> Maybe.andThen identity |> Maybe.withDefault (state, Nothing, Cmd.none)
    )

processHit oof =
    let a = Debug.log "COLLISION" "OOF"
    in oof