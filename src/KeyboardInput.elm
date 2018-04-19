module KeyboardInput exposing (..)

import Keyboard exposing (KeyCode)

type alias KeyboardInputs = List Key

type KeyFlag = Holding | Pressed
type Key = Key KeyCode KeyFlag

type KeyboardEvent = Up KeyCode | Down KeyCode

update : KeyboardEvent -> KeyboardInputs -> KeyboardInputs
update e keys =
    let newKeys = List.map (\(Key code _) -> Key code Holding) keys
    in
        case e of
            Up keyCode -> List.filter (not << matchKey keyCode) newKeys
            Down keyCode ->
                if not (List.member (Key keyCode Holding) newKeys)
                    then Key keyCode Pressed :: newKeys
                    else newKeys

subs : Sub KeyboardEvent
subs = Sub.batch
    [ Keyboard.downs Down
    , Keyboard.ups Up
    ]

matchKey : KeyCode -> Key -> Bool
matchKey code (Key c _) = code == c

keyDown : KeyCode -> KeyboardInputs -> Bool
keyDown code inputs =
    case inputs of
        [] -> False
        _  -> List.any (matchKey code) inputs

setHoldingAll : KeyboardInputs -> KeyboardInputs
setHoldingAll = List.map (\(Key code _) -> Key code Holding)