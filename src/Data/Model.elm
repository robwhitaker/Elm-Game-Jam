module Data.Model exposing (..)

import Data.Types exposing (..)
import Data.Components exposing (..)

import Time exposing (Time)
import Color

type alias Model =
    { entities : List (List Component)
    , tick : Time
    , keys : KeyboardInputs
    }

empty : Model
empty =
    { entities =
        [ [ PlayerController -- the player
          , Position (0, 0)
          , Physics (0, 0) (Just -5000)
          , Graphic 25 25 Color.red 1
          ]
        , [ PlayerController
          , Position (100, 0)
          , Physics (0, 0) (Just -15000)
          , Graphic 50 50 Color.yellow 0
          ]
        , [ Position (0, 0) -- ground
          , Graphic 1000 30 Color.green -1
          ]
        ]
    , tick = 0
    , keys = []
    }

type alias Player =
    { position : Point
    , velocity : Vec2
    }