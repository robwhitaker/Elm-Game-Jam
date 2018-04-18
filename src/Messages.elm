module Messages exposing (..)

import Time exposing (Time)
import Window exposing (Size)

import Data.Types exposing (..)
import Resource

type Msg
    = NoOp
    | Tick Time
    | KeyboardEvent KeyboardEvent
    | WindowResize Size
    | LoadTexture Resource.LoaderMsg

