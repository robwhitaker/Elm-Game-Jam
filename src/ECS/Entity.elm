module ECS.Entity exposing (..)

import ECS
import ECS.Components.Simple exposing (..)
import ECS.Components.Collision exposing (Collision)
import ECS.Components.Spritesheet exposing (Spritesheet)
import ECS.Components.PlayerController exposing (PlayerController)

type alias Updater a = ECS.Updater a Entity

type alias Entity =
    { playerController : Maybe PlayerController
    , position : Maybe Position
    , physics : Maybe Physics
    , graphic : Maybe Graphic
    , direction : Maybe Direction
    , spritesheet : Maybe Spritesheet
    , collision : Maybe Collision
    }

noComponents : Entity
noComponents =
    { playerController = Nothing
    , position = Nothing
    , physics = Nothing
    , graphic = Nothing
    , direction = Nothing
    , spritesheet = Nothing
    , collision = Nothing
    }

---- UPDATER FUNCTIONS ----

playerController_ : Updater PlayerController
playerController_ f cSet = { cSet | playerController = f cSet.playerController }

position_ : Updater Position
position_ f cSet = { cSet | position = f cSet.position }

physics_ : Updater Physics
physics_ f cSet = { cSet | physics = f cSet.physics }

graphic_ : Updater Graphic
graphic_ f cSet = { cSet | graphic = f cSet.graphic }

direction_ : Updater Direction
direction_ f cSet = { cSet | direction = f cSet.direction }

spritesheet_ : Updater Spritesheet
spritesheet_ f cSet = { cSet | spritesheet = f cSet.spritesheet }

collision_ : Updater Collision
collision_ f cSet = { cSet | collision = f cSet.collision }