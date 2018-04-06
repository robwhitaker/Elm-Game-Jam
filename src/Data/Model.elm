module Data.Model exposing (..)

import Data.Types exposing (..)
import Data.Components exposing (..)

import Time exposing (Time)
import Dict exposing (Dict)

type alias Model =
    { entities : Dict Id ComponentSet
    , tick : Time
    , keys : KeyboardInputs
    , cId  : Int
    }

empty : Model
empty =
    { entities = Dict.empty
    , tick = 0
    , keys = []
    , cId = 0
    }