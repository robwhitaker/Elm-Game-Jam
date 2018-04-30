module EnemySpawner exposing (newWave, runSpawner, getEnemiesOnScreen, getTotalRemainingEnemies)

import Random exposing (Seed)
import Time exposing (Time)
import Dict
import Vector3 as V3

import ECS
import ECS.Entity exposing (..)
import ECS.Components.Simple exposing (..)
import ECS.Components.Collision exposing (..)
import ECS.Components.Spritesheet exposing (..)
import ECS.Components.AIController exposing (..)
import ECS.Components.AudioPlayer exposing (..)
import Data.State exposing (..)
import Data.AudioConstants exposing (..)

newWave : Int -> EnemySpawner -> EnemySpawner
newWave wave spawner =
    let baseline = 5
        onScreen = 1
    in
        { spawner
            | enemiesRemaining = baseline + ceiling (toFloat wave * 0.5 * toFloat baseline)
            , maxOnScreen = onScreen + ceiling (toFloat wave * 0.3 * toFloat onScreen)
        }

getEnemiesOnScreen : State -> Int
getEnemiesOnScreen = List.length << List.filterMap (ECS.with .aiController) << Dict.values << .entities

getTotalRemainingEnemies : State -> Int
getTotalRemainingEnemies state =
    getEnemiesOnScreen state + state.enemySpawner.enemiesRemaining

runSpawner : Time -> State -> State
runSpawner dt state =
    let enemiesOnScreen = getEnemiesOnScreen state
    in
        if state.enemySpawner.spawnCD - dt <= 0 || enemiesOnScreen < 1
            then
                let screenW = 1280
                    playerX =
                        ECS.getEntityBySimpleName "player" state
                            |> Maybe.andThen (ECS.with .position >> ECS.processEntity (\(Position pos) -> V3.getX pos))
                            |> Maybe.withDefault 0
                    spawnNumberGen = Random.int 0 <| min state.enemySpawner.enemiesRemaining (state.enemySpawner.maxOnScreen - enemiesOnScreen)
                    spawnLocationGen = Random.bool |> Random.map (\b -> if b then playerX-screenW/1.5 else playerX+screenW/1.5)
                    eSpawnPosGen x = Random.float (x - 150) (x + 150) |> Random.map (\newX -> Position (newX, baseHeight, 0.9))
                    eSpeedGen = Random.float 200 500
                    eAtkSpeedGen = Random.float 0.35 0.75
                    eAtkCDGen = Random.float 0.35 1
                    newEnemyListGen n x =
                        Random.list n <| Random.map4 (swordsman state) (eSpawnPosGen x) eSpeedGen eAtkSpeedGen eAtkCDGen
                    enemyGen =
                        spawnNumberGen
                            |> Random.andThen (\spawnN -> spawnLocationGen
                            |> Random.andThen (\spawnPos -> newEnemyListGen spawnN spawnPos
                            |> Random.andThen (\newEnemies -> Random.float 2 8
                            |> Random.map (\cd ->
                                (newEnemies, spawnN, cd)
                            ))))
                    ((newEnemies, spawnN, cd), newSeed) = Random.step enemyGen state.randomSeed
                    spawner = state.enemySpawner
                in
                    List.foldl ECS.addEntity
                        { state
                            | enemySpawner = { spawner | enemiesRemaining = spawner.enemiesRemaining - spawnN, spawnCD = cd }
                            , randomSeed = newSeed
                        }
                        newEnemies
            else
                { state
                    | enemySpawner =
                        let spawner = state.enemySpawner
                        in
                            { spawner | spawnCD = spawner.spawnCD - dt }
                    }

swordsman : State -> Position -> Float -> Float -> Float -> Entity
swordsman state pos moveSpeed attackSpeed attackCD =
    noComponents
        |> ECS.set aiController_ (AIController Swordsman)
        |> ECS.set position_ pos
        |> ECS.set physics_ (Physics (0, 0) (Just -5000))
        |> ECS.set direction_ Left
        |> ECS.set hp_ (HP 25 25)
        |> ECS.set speed_ (Speed moveSpeed)
        |> ECS.set collision_ (Collision Enemy)
        |> ECS.set attackCD_ (AttackCD 0 attackCD)
        |> ECS.set audioPlayer_ (emptyAudioPlayer
                |> registerClip "gothit" 1 False False playerHits
                |> registerClip "attack" 1 False False (if attackSpeed <= 0.5 then fastSwordSwishes else slowSwordSwishes)
                |> registerClip "footstep" 1 False False footsteps
            )
        |> ECS.set spritesheet_
            (makeSpritesheet "/assets/img/e-swordsman-spritesheet.png" "idle"
                [ { animationInit
                    | name = "idle"
                    , stripDimensions = (1975, 154)
                    , numberOfFrames = 25
                    , duration = 1
                    , pivot = (0.3, 0)
                    , hitboxes =
                        [ [ Hitbox (Rect (0.1,0) (0.6,0.9)) (Hurtbox 1) ] ]
                    }
                , { animationInit
                    | name = "running"
                    , stripDimensions = (3036, 169)
                    , numberOfFrames = 23
                    , duration = 1.8 - (moveSpeed/500)
                    , pivot = (0.3, 0)
                    , hitboxes =
                        [ [ Hitbox (Rect (0.2,0) (0.6,0.8)) (Hurtbox 1) ] ]
                    }
                , { animationInit
                    | name = "attack"
                    , stripDimensions = (2385, 190)
                    , numberOfFrames = 15
                    , duration = attackSpeed
                    , pivot = (0.3, 0)
                    , loop = Change "idle"
                    , hitboxes =
                        List.repeat 6 [ Hitbox (Rect (0, 0.05) (0.3, 0.75)) (Hurtbox 1) ]
                        ++
                            [[ Hitbox (Rect (0.1, 0.05) (0.4, 0.70)) (Hurtbox 1)
                            , Hitbox (Rect (0.4, 0.5) (0.65, 1)) (Damagebox 1)
                            ]]
                        ++
                            [[ Hitbox (Rect (0.1, 0.05) (0.45, 0.70)) (Hurtbox 1)
                            , Hitbox (Rect (0.4, 0.3) (0.9, 0.8)) (Damagebox 1)
                            ]]
                        ++
                            [[ Hitbox (Rect (0.1, 0.05) (0.45, 0.70)) (Hurtbox 1)
                            , Hitbox (Rect (0.4, 0.3) (1, 0.6)) (Damagebox 1)
                            ]]
                        ++ List.repeat 2
                            [ Hitbox (Rect (0.1, 0.05) (0.45, 0.6)) (Hurtbox 1)
                            , Hitbox (Rect (0.4, 0) (0.9, 0.3)) (Damagebox 1)
                            ]
                        ++ [[ Hitbox (Rect (0.05, 0.05) (0.45, 0.7)) (Hurtbox 1) ]]
                    }
                ] state)