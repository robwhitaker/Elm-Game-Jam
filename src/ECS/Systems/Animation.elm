module ECS.Systems.Animation exposing (animation)

import ECS
import ECS.Entity exposing (..)
import ECS.Components.Spritesheet exposing (..)
import Data.State exposing (System)

import Dict

animation : System msg
animation dt model =
    let newEntities =
        Dict.map (\_ entity ->
            entity.spritesheet
                |> Maybe.andThen getRunningAnimation
                |> Maybe.andThen (\ra ->
                    let ca = ra.currentAnimation
                    in
                        ECS.update spritesheet_ (\spritesheet ->
                            let setRA =
                                if ra.timeBeforeNextFrame - dt > 0
                                    then setRunningAnimation <| Just { ra | timeBeforeNextFrame = ra.timeBeforeNextFrame - dt }
                                else if ra.currentFrame + 1 < ca.numberOfFrames
                                    then setRunningAnimation <|
                                        Just
                                            { ra
                                                | currentFrame = ra.currentFrame + 1
                                                , timeBeforeNextFrame = (ca.duration / toFloat ca.numberOfFrames) + (ra.timeBeforeNextFrame - dt)
                                            }
                                else
                                    case ca.loop of
                                        Once ->
                                            setRunningAnimation Nothing
                                        Loop ->
                                            setRunningAnimation <|
                                                Just
                                                    { ra
                                                        | currentFrame = (ra.currentFrame + 1) % ca.numberOfFrames
                                                        , timeBeforeNextFrame = (ca.duration / toFloat ca.numberOfFrames) + (ra.timeBeforeNextFrame - dt)
                                                    }
                                        Change animKey ->
                                            loadRunningAnimation animKey
                            in
                                ECS.map setRA spritesheet
                    ) entity |> Just
                ) |> Maybe.withDefault entity
            ) model.entities
    in
        ({ model | entities = newEntities }, Cmd.none)