module ECS.Components.Spritesheet exposing (..)

import Dict exposing (Dict)
import Time exposing (Time)
import Vector2 as V2 exposing (Float2)
import WebGL.Texture as Texture

import Resource exposing (ResourceDB)

type Spritesheet = Spritesheet String (Maybe RunningAnimation) (Dict String Animation) -- texturePath runningAnimation animations

type AnimationLoop = Once | Loop | Change String

type alias Animation =
    { size : Float2
    , bottomLeft : Float2
    , topRight : Float2
    , rotation : Float
    , pivot : Float2
    , numberOfFrames : Int
    , duration : Time
    , loop : AnimationLoop
    }

type alias RunningAnimation =
    { currentFrame : Int
    , timeBeforeNextFrame : Time
    , currentAnimation : Animation
    , name : String
    }

type alias AnimationInit =
    { name : String
    , scale : Float
    , stripDimensions : Float2
    , numberOfFrames : Int
    , duration : Float
    , loop : AnimationLoop
    }

animationInit : AnimationInit
animationInit =
    { name = ""
    , scale = 1
    , stripDimensions = (0, 0)
    , numberOfFrames = 1
    , duration = 1
    , loop = Loop
    }

getRunningAnimation : Spritesheet -> Maybe RunningAnimation
getRunningAnimation (Spritesheet _ runningAnimation _) =
    runningAnimation

getRunningAnimationName : Spritesheet -> Maybe String
getRunningAnimationName = getRunningAnimation >> Maybe.andThen (Just << .name)

setRunningAnimation : Maybe RunningAnimation -> Spritesheet -> Spritesheet
setRunningAnimation maybeRunningAnimation (Spritesheet texturePath _ animations) =
    Spritesheet texturePath maybeRunningAnimation animations

makeSpritesheet : String -> String -> List AnimationInit -> ResourceDB o -> Spritesheet
makeSpritesheet filePath currentAnimation animations resourceDB =
    case Resource.getTexture filePath resourceDB of
        Nothing -> Spritesheet filePath Nothing Dict.empty -- TODO: this should probably return a Maybe type
        Just tex ->
            let (tw, th) = Texture.size tex |> (\(w, h) -> (toFloat w, toFloat h))
                (_, animationDict) =
                    List.foldl (\anim (topOffset, animDict) ->
                        let sw = (V2.getX anim.stripDimensions)
                            sh = V2.getY anim.stripDimensions
                        in
                            ( topOffset + sh
                            , Dict.insert anim.name
                                { size = (anim.scale * sw / toFloat anim.numberOfFrames, anim.scale * sh)
                                , bottomLeft = (0, (th-topOffset-sh)/th)
                                , topRight = (sw/tw,(th-topOffset)/th)
                                , rotation = 0
                                , pivot = (0,0)
                                , numberOfFrames = anim.numberOfFrames
                                , duration = anim.duration
                                , loop = anim.loop
                                } animDict
                            )
                    ) (0, Dict.empty) animations
            in
                Spritesheet filePath Nothing animationDict |> loadRunningAnimation currentAnimation

loadRunningAnimation : String -> Spritesheet -> Spritesheet
loadRunningAnimation currentAnimation (Spritesheet filePath runningAnimation animations as spritesheet) =
    if Just currentAnimation == Maybe.map .name runningAnimation
        then spritesheet
        else
            case Dict.get currentAnimation animations of
                Nothing -> Spritesheet filePath Nothing animations
                Just animation ->
                    Spritesheet filePath
                                (Just
                                    { currentFrame = 0
                                    , timeBeforeNextFrame = animation.duration / toFloat animation.numberOfFrames
                                    , currentAnimation = animation
                                    , name = currentAnimation
                                    }
                                )
                                animations

mapCurrentAnimation : (Animation -> Animation) -> Spritesheet -> Spritesheet
mapCurrentAnimation f (Spritesheet filePath maybeRunningAnimation animations as spritesheet) =
    case maybeRunningAnimation of
        Nothing -> spritesheet
        Just runningAnimation ->
            Spritesheet filePath (Just { runningAnimation | currentAnimation = f runningAnimation.currentAnimation }) animations

mapAnimations : (Dict String Animation -> Dict String Animation) -> Spritesheet -> Spritesheet
mapAnimations f (Spritesheet filePath runningAnimation animations) =
    Spritesheet filePath runningAnimation (f animations)