module ECS.Systems.AI exposing (ai)

import ECS
import ECS.Entity exposing (..)
import ECS.Components.Simple exposing (..)
import ECS.Components.PlayerController exposing (..)
import ECS.Components.Spritesheet exposing (..)
import ECS.Components.AIController exposing (..)
import ECS.Components.AudioPlayer exposing (..)
import Data.State exposing (System, State)
import KeyboardInput exposing (..)

import Dict
import Time exposing (Time)
import Vector2 as V2
import Vector3 as V3

ai : System msg
ai dt =
    ECS.processEntities (\state id entity ->
        entity
            |> ECS.with .aiController
            |> ECS.processEntity (\(AIController enemyType) ->
                case enemyType of
                    Swordsman -> swordsmanAI dt state entity
            ) |> Maybe.withDefault Nothing |> Maybe.withDefault (state, Nothing, Cmd.none)
    )

swordsmanAI : Time -> State -> Entity -> Maybe (State, Maybe Entity, Cmd msg)
swordsmanAI dt state entity =
    ECS.getEntityBySimpleName "player" state
        |> Maybe.andThen (\player ->
            entity
                |> ECS.with .position
                |> ECS.andWith .direction
                |> ECS.andWith .attackCD
                |> ECS.andWith .speed
                |> ECS.andWith .spritesheet
                |> ECS.andWith (always player.position)
                |> ECS.andWith (always player.direction)
                |> ECS.andWith (always player.spritesheet)
                |> ECS.processEntity
                    (\(Position (ex, ey,_)) edir (AttackCD cd maxCD as atkcd) (Speed eSpeed) eSpritesheet (Position (px,py,_)) pdir pSpritesheet ->
                        let dirShouldBe = toDir (px - ex)
                            moveSpeed = eSpeed
                            cdLeft = cd - dt
                        in
                            getRunningAnimation eSpritesheet
                                |> Maybe.andThen (\ra ->
                                    (\e -> Just (state, Just e, Cmd.none)) <|
                                        let sfxEntity =
                                            if ra.name == "running" && ra.currentFrame % 4 == 0
                                                then ECS.update audioPlayer_ (Maybe.map (queueAudio "footstep")) entity
                                                else entity
                                        in
                                            if abs (px - ex) > 75 && ra.name /= "attack"
                                                then
                                                    sfxEntity
                                                        |> ECS.update physics_ (Maybe.map (\(Physics (vx, vy) g) ->
                                                                Physics (turn dirShouldBe moveSpeed, vy) g
                                                            ))
                                                        |> ECS.set direction_ dirShouldBe
                                                        |> ECS.set attackCD_ (AttackCD cdLeft maxCD)
                                                        |> ECS.update spritesheet_ (Maybe.map (loadRunningAnimation "running"))
                                                else if ra.name /= "attack" && cdLeft <= 0
                                                    then sfxEntity
                                                        |> ECS.update physics_ (Maybe.map (\(Physics (vx, vy) g) ->
                                                                Physics (0, vy) g
                                                            ))
                                                        |> ECS.set direction_ dirShouldBe
                                                        |> ECS.update spritesheet_ (Maybe.map (loadRunningAnimation "attack"))
                                                else if ra.name == "attack" && ra.currentFrame >= 7 && ra.currentFrame < ra.currentAnimation.numberOfFrames - 1
                                                    then ECS.update audioPlayer_ (Maybe.map (queueAudio "attack")) sfxEntity
                                                else if ra.name == "attack" && ra.currentFrame == ra.currentAnimation.numberOfFrames - 1 && cdLeft <= 0
                                                    then ECS.set attackCD_ (AttackCD maxCD maxCD) sfxEntity
                                                else
                                                    sfxEntity
                                                        |> ECS.set attackCD_ (AttackCD cdLeft maxCD)
                                    )
                                |> Maybe.withDefault (state, Nothing, Cmd.none)
                    )
        )