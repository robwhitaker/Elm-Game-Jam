module Init exposing (..)

import Data.State exposing (..)
import Data.AudioConstants exposing (..)
import ECS
import ECS.Entity exposing (..)
import ECS.Components.Simple exposing (..)
import ECS.Components.Collision exposing (..)
import ECS.Components.PlayerController exposing (..)
import ECS.Components.Spritesheet exposing (..)
import ECS.Components.AIController exposing (..)
import ECS.Components.AudioPlayer exposing (..)
import Resource
import Messages exposing (..)

import Dict

entities : Bool -> State -> State
entities startBgm state =
    { state | entities = Dict.empty, entitySimpleNames = Dict.empty }
        |> ECS.addEntityWithSimpleName (Just "player") ( noComponents
                        |> ECS.set playerController_ (PlayerController Idle)
                        |> ECS.set position_ (Position (0, 0, 1))
                        |> ECS.set physics_ (Physics (0, 0) (Just -5000))
                        |> ECS.set direction_ Right
                        |> ECS.set hp_ (HP 25 25)
                        |> ECS.set speed_ (Speed 750)
                        |> ECS.set collision_ (Collision Player)
                        |> ECS.set audioPlayer_ (emptyAudioPlayer
                                |> registerClip "gothit" 1 False False enemyHits
                                |> registerClip "attack" 1 False False fastSwordSwishes
                                |> registerClip "footstep" 1 False False footsteps
                            )
                        |> ECS.set spritesheet_
                            (makeSpritesheet "/assets/img/player-spritesheet.png" "idle"
                                [ { animationInit
                                    | name = "idle"
                                    , stripDimensions = (1975, 154)
                                    , numberOfFrames = 25
                                    , duration = 1
                                    , pivot = (0.3, 0)
                                    , hitboxes =
                                        [ [ Hitbox (Rect (0.1,0) (0.6,0.9)) (Hurtbox 1) ] ]
                                    }
                                , { animationInit
                                    | name = "running"
                                    , stripDimensions = (3036, 169)
                                    , numberOfFrames = 23
                                    , duration = 0.8
                                    , pivot = (0.3, 0)
                                    , hitboxes =
                                        [ [ Hitbox (Rect (0.2,0) (0.6,0.8)) (Hurtbox 1) ] ]
                                    }
                                , { animationInit
                                    | name = "attack"
                                    , stripDimensions = (2385, 190)
                                    , numberOfFrames = 15
                                    , duration = 0.30
                                    , pivot = (0.3, 0)
                                    , hitboxes =
                                        List.repeat 6 [ Hitbox (Rect (0, 0.05) (0.3, 0.75)) (Hurtbox 1) ]
                                        ++
                                            [[ Hitbox (Rect (0.1, 0.05) (0.4, 0.70)) (Hurtbox 1)
                                            , Hitbox (Rect (0.4, 0.5) (0.65, 1)) (Damagebox 1)
                                            ]]
                                        ++
                                            [[ Hitbox (Rect (0.1, 0.05) (0.45, 0.70)) (Hurtbox 1)
                                            , Hitbox (Rect (0.4, 0.3) (0.9, 0.8)) (Damagebox 2)
                                            ]]
                                        ++
                                            [[ Hitbox (Rect (0.1, 0.05) (0.45, 0.70)) (Hurtbox 1)
                                            , Hitbox (Rect (0.4, 0.3) (1, 0.6)) (Damagebox 5)
                                            ]]
                                        ++ List.repeat 2
                                            [ Hitbox (Rect (0.1, 0.05) (0.45, 0.6)) (Hurtbox 1)
                                            , Hitbox (Rect (0.4, 0) (0.9, 0.3)) (Damagebox 1)
                                            ]
                                        ++ [[ Hitbox (Rect (0.05, 0.05) (0.45, 0.7)) (Hurtbox 1) ]]
                                    }
                                ] state) )
        |> ECS.addEntityWithSimpleName (Just "jukebox") (noComponents
                |> ECS.set audioPlayer_ (emptyAudioPlayer
                        |> registerClip "bgm" 0.5 True False [ bgm ]
                        |> if startBgm then queueAudio "bgm" else identity
                    )
            )

resources : (Resource.ResourceDB o, Cmd Msg) -> (Resource.ResourceDB o, Cmd Msg)
resources =
    let loadAudio = Resource.loadAudio ["ogg", "m4a"]
    in
        Resource.initLoader <|
            [ Resource.loadTexture LoadResource "/assets/img/cloud-bg.png"
            , Resource.loadTexture LoadResource "/assets/img/terrible-trees.png"
            , Resource.loadTexture LoadResource "/assets/img/ground.png"
            , Resource.loadTexture LoadResource "/assets/img/player-spritesheet.png"
            , Resource.loadTexture LoadResource "/assets/img/e-swordsman-spritesheet.png"
            , loadAudio bgm
            ] ++ List.map loadAudio allSounds