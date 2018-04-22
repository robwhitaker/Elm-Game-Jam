module Init exposing (..)

import Data.State exposing (..)
import ECS
import ECS.Entity exposing (..)
import ECS.Components.Simple exposing (..)
import ECS.Components.PlayerController exposing (..)
import ECS.Components.Spritesheet exposing (..)
import Resource
import Messages exposing (..)

import Color

entities : State -> State
entities model =
    model
        |> ECS.addEntityWithSimpleName (Just "player") ( noComponents
                        |> ECS.set playerController_ (PlayerController Idle)
                        |> ECS.set position_ (Position (100, 0, 1))
                        |> ECS.set physics_ (Physics (0, 0) (Just -5000))
                        |> ECS.set direction_ Right
                        |> ECS.set spritesheet_
                            (makeSpritesheet "/assets/img/player-spritesheet.png" "idle"
                                [ { animationInit
                                    | name = "idle"
                                    , stripDimensions = (1975, 154)
                                    , numberOfFrames = 25
                                    , duration = 1
                                    , pivot = (0.5, 0)
                                    }
                                , { animationInit
                                    | name = "running"
                                    , stripDimensions = (3036, 169)
                                    , numberOfFrames = 23
                                    , duration = 0.8
                                    , pivot = (0.5, 0)
                                    }
                                , { animationInit
                                    | name = "attack"
                                    , stripDimensions = (2385, 190)
                                    , numberOfFrames = 15
                                    , duration = 0.25
                                    , pivot = (0.35, 0)
                                    }
                                ] model) )
        |> ECS.addEntity ( noComponents -- ground
                        |> ECS.set position_ (Position (-5000, -975, 0))
                        |> ECS.set graphic_ (Graphic 10000 1000 Color.green) )

resources : (Resource.ResourceDB o, Cmd Msg) -> (Resource.ResourceDB o, Cmd Msg)
resources =
    Resource.initLoader
        [ Resource.loadTexture LoadTexture "/assets/img/temp_bg.png"
        , Resource.loadTexture LoadTexture "/assets/img/player-spritesheet.png"
        ]