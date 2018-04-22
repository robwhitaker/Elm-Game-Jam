module ECS.Systems.Animation exposing (animation)

import ECS
import ECS.Entity exposing (..)
import ECS.Components.Simple exposing (..)
import ECS.Components.Spritesheet exposing (..)
import Data.State exposing (System)

import Vector2 as V2

animation : System msg
animation dt =
    ECS.processEntities (\state _ entity ->
        entity
            |> ECS.with .spritesheet
            |> ECS.andWith .direction
            |> ECS.processEntity (\spritesheet direction ->
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
                            setRAWithDir =
                                setRA
                                    >> mapCurrentAnimation (\cAnim ->
                                        { cAnim
                                            | size = (,)
                                                (turn direction (V2.getX cAnim.size))
                                                (V2.getY cAnim.size)
                                        })
                        in
                            Just <| ECS.set spritesheet_ (setRAWithDir spritesheet) entity
                        )
                ) |> ECS.component (state, Nothing, Cmd.none) (\e -> (state, e, Cmd.none))
        )