module Data.Model exposing (..)

import Data.Types exposing (..)
import Data.ComponentTable exposing (..)
import Data.ECS as ECS

import Time exposing (Time)
import Dict exposing (Dict)
import Game.TwoD.Camera as Camera exposing (Camera)
import Vector2 exposing (Vec2)
import WebGL.Texture exposing (Texture)

type alias Model =
    ECS.State ComponentTable
        { tick : Time
        , keys : KeyboardInputs
        , camera : Camera
        , windowSize : Vec2 Int
        , resources : Dict String Texture
        }

empty : Model
empty =
    { entities = Dict.empty
    , entitySimpleNames = Dict.empty
    , cId = 0
    , tick = 0
    , keys = []
    , camera = Camera.fixedWidth 1280 (0,0)
    , windowSize = (0, 0)
    , resources = Dict.empty
    }