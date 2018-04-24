module ECS.Components.Collision exposing (..)

import Collision2D exposing (Rectangle)
import Vector2 as V2 exposing (Float2)
import Vector3 as V3 exposing (Float3)

type Collision = Collision CollisionClass

type CollisionClass
    = Player
    | Enemy

type Hitbox
    = Hitbox Rect
    | Hurtbox Rect

type alias Rect =
    { bottomLeft : (Float, Float)
    , topRight : (Float, Float)
    }

type alias WorldRect =
    { position : Float3
    , size : Float2
    , collisionRect : Rectangle
    }

getRect : Hitbox -> Rect
getRect hbox =
    case hbox of
        Hitbox r -> r
        Hurtbox r -> r

worldRect : Float3 -> Float2 -> Float2 -> Rect -> WorldRect
worldRect (px, py, pz) (sx, sy) (pivX, pivY) rect =
    let
        xMult = (Tuple.first rect.topRight - Tuple.first rect.bottomLeft)
        yMult = (Tuple.second rect.topRight - Tuple.second rect.bottomLeft)
        rsx = xMult * sx
        rsy = yMult * sy

        rpx = px - pivX*sx + ((1 - xMult)*sx) - (1 - Tuple.first rect.topRight)*sx
        rpy = py - pivY*sy + ((1 - yMult)*sy) - (1 - Tuple.second rect.topRight)*sy
    in
        { position = (rpx, rpy, pz - 0.01)
        , size = (rsx, rsy)
        , collisionRect = Collision2D.rectangle rpx rpy rsx rsy
        }