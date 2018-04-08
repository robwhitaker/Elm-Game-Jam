module Messages exposing (..)

import Time exposing (Time)
import Window exposing (Size)

import WebGL.Texture exposing (Error, Texture)

import Data.Types exposing (..)

type Msg
    = NoOp
    | Tick Time
    | KeyboardEvent KeyboardEvent
    | WindowResize Size
    | TextureLoad (Result Error Texture)

