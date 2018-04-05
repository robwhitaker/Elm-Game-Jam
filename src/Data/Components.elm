module Data.Components exposing (..)

import Color exposing (Color)

import Data.Types exposing (..)

type Component
    = Disabled Component
    | PlayerController
    | Position Point
    | Physics Vec2 (Maybe Float)
    | Graphic Float Float Color Int

position : Component -> Bool
position component =
    case component of
        (Position _) -> True
        _ -> False

physics : Component -> Bool
physics component =
    case component of
        (Physics _ _) -> True
        _ -> False

graphic : Component -> Bool
graphic component =
    case component of
        (Graphic _ _ _ _) -> True
        _ -> False

playerController : Component -> Bool
playerController component =
    case component of
        PlayerController -> True
        _ -> False

getComponent : (Component -> Bool) -> List Component -> Maybe Component
getComponent pred = List.head << List.filter pred

updateComponent : (Component -> Bool) -> Component -> List Component -> List Component
updateComponent pred newComponent entity =
    newComponent :: List.filter (not << pred) entity

hasAllComponents : List (Component -> Bool) -> List Component -> Bool
hasAllComponents components entity =
    if List.length entity == 0
        then False
        else
            (==) 0 <| List.length <|
                List.filter (\pred -> not <| List.any pred entity) components