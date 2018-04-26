module ECS.Components.Simple exposing (..)

import Color exposing (Color)
import Vector2 exposing (Float2)
import Vector3 exposing (Float3)

type Position = Position Float3
type Physics = Physics Float2 (Maybe Float)
type Graphic = Graphic Float Float Color
type HP = HP Float Float -- current max

type Direction = Left | Right

turn : Direction -> Float -> Float
turn d x =
    dir d * abs x

toDir : Float -> Direction
toDir x =
    if x < 0 then Left else Right

dir : Direction -> Float
dir d =
    case d of
        Left -> -1
        Right -> 1