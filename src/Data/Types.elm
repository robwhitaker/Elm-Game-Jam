module Data.Types exposing (..)

import Keyboard exposing (KeyCode)

type alias Point = (Float, Float)

type alias Vec2 = (Float, Float)

type KeyboardEvent = Up KeyCode | Down KeyCode

type KeyFlag = Holding | Pressed

type Key = Key KeyCode KeyFlag

type alias KeyboardInputs = List Key