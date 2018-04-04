module Messages exposing (..)

import Time exposing (Time)

import Data.Types exposing (..)

type Msg
    = NoOp
    | Tick Time
    | KeyboardEvent KeyboardEvent

