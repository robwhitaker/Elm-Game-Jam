module Data.ComponentTable exposing (..)

import Color exposing (Color)
import Time exposing (Time)
import Vector2 as V2 exposing (Float2, Vec2)
import Vector3 as V3 exposing (Float3)
import Dict exposing (Dict)

import Data.ECS as ECS

type alias Updater a = ECS.Updater a ComponentTable

type alias ComponentTable =
    { playerController : Maybe PlayerController
    , position : Maybe Position
    , physics : Maybe Physics
    , graphic : Maybe Graphic
    , spritesheet : Maybe Spritesheet
    }

noComponents : ComponentTable
noComponents =
    { playerController = Nothing
    , position = Nothing
    , physics = Nothing
    , graphic = Nothing
    , spritesheet = Nothing
    }

type PlayerController = PlayerController
type Position = Position Float3
type Physics = Physics (Vec2 Float) (Maybe Float)
type Graphic = Graphic Float Float Color
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

getRunningAnimation : Spritesheet -> Maybe RunningAnimation
getRunningAnimation (Spritesheet _ runningAnimation _) =
    runningAnimation

getRunningAnimationName : Spritesheet -> Maybe String
getRunningAnimationName = getRunningAnimation >> Maybe.andThen (Just << .name)

setRunningAnimation : Maybe RunningAnimation -> Spritesheet -> Spritesheet
setRunningAnimation maybeRunningAnimation (Spritesheet texturePath _ animations) =
    Spritesheet texturePath maybeRunningAnimation animations

makeSpritesheet : String -> String -> Dict String Animation -> Spritesheet
makeSpritesheet filePath currentAnimation animations =
    Spritesheet filePath Nothing animations |> loadRunningAnimation currentAnimation

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

---- UPDATER FUNCTIONS ----

playerController_ : Updater PlayerController
playerController_ f cSet = { cSet | playerController = f cSet.playerController }

position_ : Updater Position
position_ f cSet = { cSet | position = f cSet.position }

physics_ : Updater Physics
physics_ f cSet = { cSet | physics = f cSet.physics }

graphic_ : Updater Graphic
graphic_ f cSet = { cSet | graphic = f cSet.graphic }

spritesheet_ : Updater Spritesheet
spritesheet_ f cSet = { cSet | spritesheet = f cSet.spritesheet }