module Data.State exposing
    ( System, State, empty )

import ECS
import ECS.Entity exposing (Entity)
import KeyboardInput exposing (KeyboardInputs)
import Resource

import Dict
import Game.TwoD.Camera as Camera exposing (Camera)
import Vector2 exposing (Vec2)

-- Some specialized types

type alias State =
    ECS.State Entity Model

type alias System msg =
    ECS.System Entity Model msg

type alias Model =
    Resource.ResourceDB
        { keys : KeyboardInputs
        , camera : Camera
        , windowSize : Vec2 Int
        }

empty : State
empty =
    { entities = Dict.empty
    , entitySimpleNames = Dict.empty
    , cId = 0
    , keys = []
    , camera = Camera.fixedWidth 1280 (0,0)
    , windowSize = (0, 0)
    , resourceLoader = Resource.loader
    }