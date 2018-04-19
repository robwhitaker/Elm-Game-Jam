module ECS.Systems.Physics exposing (physics)

import ECS
import ECS.Entity exposing (..)
import ECS.Components.Simple exposing (..)
import Data.State exposing (System)

import Dict

physics : System msg
physics dt model =
    let newEntities =
        Dict.map (\_ entity ->
            let applyPhysics (Position (x, y, z)) (Physics (vx, vy) mGravity) =
                    let vyNew = vy + (Maybe.withDefault 0 mGravity * dt)
                        xNew = vx * dt + x
                        yNew = vyNew * dt + y
                        (y_,vy_) = if yNew <= 15 then (15, 0) else (yNew, vyNew)
                    in
                        [ ECS.set position_ <| Position (xNew, y_, z)
                        , ECS.set physics_ <| Physics (vx, vy_) mGravity
                        ]
            in
                case ECS.map2 applyPhysics entity.position entity.physics of
                    Nothing -> entity
                    Just updaters -> List.foldl (<|) entity updaters
        ) model.entities
    in
        ({ model | entities = newEntities}, Cmd.none)