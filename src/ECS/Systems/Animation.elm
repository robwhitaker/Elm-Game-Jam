module ECS.Systems.Animation exposing (animation)

import ECS
import ECS.Entity exposing (..)
import ECS.Components.Spritesheet exposing (..)
import Data.State exposing (System)

animation : System msg
animation dt =
    ECS.processEntities (\state _ entity ->
        entity
            |> ECS.with .spritesheet (\spritesheet ->
                getRunningAnimation spritesheet
                    |> Maybe.andThen (\ra ->
                        let ca = ra.currentAnimation
                            setRA =
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
                            Just <| ECS.set spritesheet_ (setRA spritesheet) entity
                        )
                ) |> ECS.component (state, Nothing, Cmd.none) (\e -> (state, e, Cmd.none))
        )