module Messages exposing (..)

import Time exposing (Time)
import Window exposing (Size)
import Random exposing (Seed)

import Resource
import KeyboardInput exposing (KeyboardEvent)

type Msg
    = NoOp
    | Tick Time
    | KeyboardEvent KeyboardEvent
    | WindowResize Size
    | LoadTexture Resource.LoaderMsg
    | RandomSeed Seed
    | NewGame

