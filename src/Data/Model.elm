module Data.Model exposing (..)

import Data.Types exposing (..)
import Data.ComponentTable exposing (..)
import Data.ECS as ECS
import Resource

import Time exposing (Time)
import Dict exposing (Dict)
import Game.TwoD.Camera as Camera exposing (Camera)
import Vector2 exposing (Vec2)

type alias Model =
    ECS.State ComponentTable
        (Resource.ResourceDB
            { tick : Time
            , keys : KeyboardInputs
            , camera : Camera
            , windowSize : Vec2 Int
            })

empty : Model
empty =
    { entities = Dict.empty
    , entitySimpleNames = Dict.empty
    , cId = 0
    , tick = 0
    , keys = []
    , camera = Camera.fixedWidth 1280 (0,0)
    , windowSize = (0, 0)
    , resourceLoader = Resource.loader
    }