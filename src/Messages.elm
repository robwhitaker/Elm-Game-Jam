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
    | LoadResource Resource.LoaderMsg
    | RandomSeed Seed
    | NewGame
    | AudioEvent AudioEvent

type AudioEvent
    = End (Int, String)
    | Stop (Int, String)