module ECS.Components.Simple exposing (..)

import Color exposing (Color)
import Vector2 exposing (Float2)
import Vector3 exposing (Float3)

type Position = Position Float3
type Physics = Physics Float2 (Maybe Float)
type Graphic = Graphic Float Float Color