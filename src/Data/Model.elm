module Data.Model exposing (..)

import Data.Types exposing (..)
import Data.Components exposing (..)

import Time exposing (Time)
import Dict exposing (Dict)
import Game.TwoD.Camera as Camera exposing (Camera)
import Vector2 exposing (Vec2)
import WebGL.Texture exposing (Texture)

type alias Model =
    { entities : Dict Id ComponentSet
    , tick : Time
    , keys : KeyboardInputs
    , cId  : Int
    , camera : Camera
    , windowSize : Vec2 Int
    , bgTexture : Maybe Texture --TODO: better handling of textures later
    }

empty : Model
empty =
    { entities = Dict.empty
    , tick = 0
    , keys = []
    , cId = 0
    , camera = Camera.fixedWidth 1280 (0,0)
    , windowSize = (0, 0)
    , bgTexture = Nothing
    }