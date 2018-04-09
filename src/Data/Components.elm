module Data.Components exposing (..)

import Color exposing (Color)
import Data.Types exposing (..)
import Time exposing (Time)
import Vector2 as V2 exposing (Float2, Vec2)
import Vector3 as V3 exposing (Float3)
import Dict exposing (Dict)

---- COMPONENT RECORD ----

type alias ComponentSet =
    { playerController : Maybe PlayerController
    , position : Maybe Position
    , physics : Maybe Physics
    , graphic : Maybe Graphic
    , spritesheet : Maybe Spritesheet
    }

noComponents : ComponentSet
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

type Animation = Animation Float2 Float2 Float2 Float Float2 Int Time -- size bottomLeft topRight rotation pivot nFrames duration
type RunningAnimation = RunningAnimation Int Time Animation -- currentFrame timeBeforeNextFrame currentAnimation

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

---- UPDATER HELPERS ----

type alias Updater a = (Maybe a -> Maybe a) -> ComponentSet -> ComponentSet

set : Updater a -> a -> ComponentSet -> ComponentSet
set updater = updater << always << Just

update : Updater a -> Updater a
update = identity

remove : Updater a -> ComponentSet -> ComponentSet
remove updater = updater (always Nothing)

---- MAPPING FUNCTIONS (re-exports from Maybe, in case of implementation change) ----

map : (a -> b) -> Maybe a -> Maybe b
map = Maybe.map

map2 : (a -> b -> value) -> Maybe a -> Maybe b -> Maybe value
map2 = Maybe.map2

map3 : (a -> b -> c -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe value
map3 = Maybe.map3

component : b -> (a -> b) -> Maybe a -> b
component default f =
    Maybe.withDefault default << map f