module Data.Model exposing (..)

import Data.Types exposing (..)
import Time exposing (Time)

type alias Model =
    { player : Player
    , tick : Time
    , keys : KeyboardInputs
    }

empty : Model
empty =
    { player =
        { position = (0, 15)
        , velocity = (0, 98)
        }
    , tick = 0
    , keys = []
    }

type alias Player =
    { position : Point
    , velocity : Vec2
    }