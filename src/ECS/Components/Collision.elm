module ECS.Components.Collision exposing (..)

import Collision2D exposing (Rectangle)
import Vector2 as V2 exposing (Float2)
import Vector3 as V3 exposing (Float3)

type Collision = Collision CollisionClass

type CollisionClass
    = Player
    | Enemy

type Hitbox = Hitbox Rect HitboxType

type HitboxType
    = Damagebox Float -- damage to do
    | Hurtbox Float -- incoming damage modifier

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
getRect (Hitbox rect _) = rect

getHitboxType : Hitbox -> HitboxType
getHitboxType (Hitbox _ hboxtype) = hboxtype

-- NOTE:  Collision2D uses origin center, whereas game 2D uses origin based on the pivot for graphics ((0,0) is bottom-left when facing right)
-- NOTE2: pivotY is ignored here since I'm not flipping things that way
worldRect : Float3 -> Float2 -> Float2 -> Rect -> WorldRect
worldRect (px, py, pz) (spriteWidth, spriteHeight) (pivotX, _) rect =
    let xMult = Tuple.first rect.topRight - Tuple.first rect.bottomLeft
        yMult = Tuple.second rect.topRight - Tuple.second rect.bottomLeft
        sw = abs spriteWidth
        sh = abs spriteHeight
        cxw = sw * xMult
        cyh = sh * yMult
        (cxGraphic, cxCollision) =
            if spriteWidth < 0
                then
                    let cxGraphic = px + (sw * pivotX) - (Tuple.first rect.topRight * sw)
                        cxCollision = cxGraphic + cxw/2
                    in
                        (cxGraphic, cxCollision)
                else
                    let cxGraphic = px - (sw * pivotX) + (Tuple.first rect.bottomLeft * sw)
                        cxCollision = cxGraphic + cxw/2
                    in
                        (cxGraphic, cxCollision)
        cyGraphic = py + (Tuple.second rect.bottomLeft * sh)
        cyCollision = cyGraphic + cyh/2
    in
        { position = (cxGraphic, cyGraphic, pz - 0.01)
        , size = (cxw, cyh)
        , collisionRect = Collision2D.rectangle cxCollision cyCollision cxw cyh
        }