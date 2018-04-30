module Data.State exposing
    ( System, State, empty, EnemySpawner, GameState(..), baseHeight)

import ECS exposing (Id)
import ECS.Entity exposing (Entity)
import KeyboardInput exposing (KeyboardInputs)
import Resource

import Dict
import Game.TwoD.Camera as Camera exposing (Camera)
import Vector2 exposing (Vec2)
import Random exposing (Seed)
import Time exposing (Time)

-- Some specialized types

type alias State =
    ECS.State Entity Model

type alias System msg =
    ECS.System Entity Model msg

type alias Model =
    Resource.ResourceDB
        { keys : KeyboardInputs
        , camera : Camera
        , windowSize : Vec2 Int
        , gameState : GameState
        , wave : Int
        , enemySpawner : EnemySpawner
        , randomSeed : Seed
        }

type GameState
    = Loading
    | Start
    | Playing
    | WaveTransition Time
    | GameOver

type alias EnemySpawner =
    { enemiesRemaining : Int
    , enemiesOnScreen : Int
    , maxOnScreen : Int
    , spawnCD : Time
    }

emptySpawner : EnemySpawner
emptySpawner =
    { enemiesRemaining = 0
    , enemiesOnScreen = 0
    , maxOnScreen = 0
    , spawnCD = 0
    }

empty : State
empty =
    { entities = Dict.empty
    , entitySimpleNames = Dict.empty
    , cId = 0
    , keys = []
    , camera = Camera.fixedWidth 1280 (0,0)
    , windowSize = (0, 0)
    , resourceLoader = Resource.loader
    , wave = 0
    , enemySpawner = emptySpawner
    , gameState = Loading
    , randomSeed = Random.initialSeed 0
    }

-- magic number for baseline height
baseHeight : Float
baseHeight = 0