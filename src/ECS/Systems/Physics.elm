module ECS.Systems.Physics exposing (physics)

import ECS
import ECS.Entity exposing (..)
import ECS.Components.Simple exposing (..)
import Data.State exposing (System)

physics : System msg
physics dt =
    ECS.processEntities (\state _ entity ->
        entity
            |> ECS.with .position
            |> ECS.andWith .physics
            |> ECS.processEntity
                (\(Position (x, y, z)) (Physics (vx, vy) mGravity) ->
                    let vyNew = vy + ((Maybe.withDefault 0 mGravity + (15-y)*10) * dt)
                        xNew = vx * dt + x
                        yNew = vyNew * dt + y
                        (y_,vy_) = if yNew <= 15 then (15, 0) else (yNew, vyNew)
                    in
                        entity
                            |> ECS.set position_ (Position (xNew, y_, z))
                            |> ECS.set physics_  (Physics (vx, vy_) mGravity)
            ) |> (\e -> (state, e, Cmd.none))
    )