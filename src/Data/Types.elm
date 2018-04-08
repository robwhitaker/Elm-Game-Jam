module Data.Types exposing (..)

import Keyboard exposing (KeyCode)

type KeyboardEvent = Up KeyCode | Down KeyCode

type KeyFlag = Holding | Pressed

type Key = Key KeyCode KeyFlag

type alias KeyboardInputs = List Key

type alias Id = Int