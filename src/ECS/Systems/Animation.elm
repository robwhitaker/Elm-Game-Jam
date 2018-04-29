module ECS.Systems.Animation exposing (animation)

import ECS
import ECS.Entity exposing (..)
import ECS.Components.Simple exposing (..)
import ECS.Components.Spritesheet exposing (..)
import Data.State exposing (System)
import Utils.SelectionList as SL

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
                            newRA =
                                if ra.timeBeforeNextFrame - dt > 0
                                    then Just { ra | timeBeforeNextFrame = ra.timeBeforeNextFrame - dt }
                                else if ra.currentFrame + 1 < ca.numberOfFrames
                                    then
                                        Just
                                            { ra
                                                | currentFrame = ra.currentFrame + 1
                                                , timeBeforeNextFrame = (ca.duration / toFloat ca.numberOfFrames) + (ra.timeBeforeNextFrame - dt)
                                            }
                                else
                                    case ca.loop of
                                        Once ->
                                            Just ra
                                        Loop ->
                                            Just
                                                { ra
                                                    | currentFrame = (ra.currentFrame + 1) % ca.numberOfFrames
                                                    , timeBeforeNextFrame = (ca.duration / toFloat ca.numberOfFrames) + (ra.timeBeforeNextFrame - dt)
                                                }
                                        Change animKey ->
                                            loadRunningAnimation animKey spritesheet |> getRunningAnimation
                            finalRA =
                                Maybe.map (\runningAnim ->
                                    let cAnim = runningAnim.currentAnimation
                                    in
                                        { runningAnim
                                            | currentAnimation =
                                                { cAnim
                                                    | size = (,)
                                                    (turn direction (V2.getX cAnim.size))
                                                    (V2.getY cAnim.size)
                                                , hitboxes = Maybe.map (SL.goto runningAnim.currentFrame) cAnim.hitboxes
                                            }
                                        }
                                ) newRA

                        in
                            Just <| ECS.set spritesheet_ (setRunningAnimation finalRA spritesheet) entity
                        )
                ) |> ECS.component (state, Nothing, Cmd.none) (\e -> (state, e, Cmd.none))
        )