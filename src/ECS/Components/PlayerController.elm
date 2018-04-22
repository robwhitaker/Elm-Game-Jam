module ECS.Components.PlayerController exposing (..)

import Time exposing (Time)

type PlayerController = PlayerController PlayerState

type PlayerState
    = Idle
    | Attacking Time
    | Running

isAttacking : PlayerState -> Bool
isAttacking pState =
    case pState of
        Attacking _ -> True
        _ -> False