module ECS.Entity exposing (..)

import ECS
import ECS.Components.Simple exposing (..)
import ECS.Components.Collision exposing (Collision)
import ECS.Components.Spritesheet exposing (Spritesheet)
import ECS.Components.PlayerController exposing (PlayerController)
import ECS.Components.AIController exposing (AIController)
import ECS.Components.AudioPlayer exposing (AudioPlayer)

type alias Updater a = ECS.Updater a Entity

type alias Entity =
    { playerController : Maybe PlayerController
    , position : Maybe Position
    , physics : Maybe Physics
    , graphic : Maybe Graphic
    , direction : Maybe Direction
    , spritesheet : Maybe Spritesheet
    , collision : Maybe Collision
    , hp : Maybe HP
    , aiController : Maybe AIController
    , attackCD : Maybe AttackCD
    , speed : Maybe Speed
    , audioPlayer : Maybe AudioPlayer
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
    , hp = Nothing
    , aiController = Nothing
    , attackCD = Nothing
    , speed = Nothing
    , audioPlayer = Nothing
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

hp_ : Updater HP
hp_ f cSet = { cSet | hp = f cSet.hp }

aiController_ : Updater AIController
aiController_ f cSet = { cSet | aiController = f cSet.aiController }

attackCD_ : Updater AttackCD
attackCD_ f cSet = { cSet | attackCD = f cSet.attackCD }

speed_ : Updater Speed
speed_ f cSet = { cSet | speed = f cSet.speed }

audioPlayer_ : Updater AudioPlayer
audioPlayer_ f cSet = { cSet | audioPlayer = f cSet.audioPlayer }