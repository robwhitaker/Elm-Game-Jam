module Main exposing (main)

import Html
import Html.Attributes exposing (style)
import Collage
import Element
import Time
import Color
import Dict exposing (Dict)
import Task

import AnimationFrame
import Keyboard
import Window

import Game.TwoD as Game
import Game.TwoD.Camera as Camera
import Game.TwoD.Render as Render
import Vector2 as V2
import Vector3 as V3
import WebGL.Texture as Texture exposing (Texture)

import Data.Model exposing (..)
import Messages exposing (..)
import Data.Types exposing (..)
import Data.ComponentTable as Component exposing (..)
import Data.ECS as ECS exposing (Id)

initModel : Model
initModel =
    empty
        |> ECS.addEntityWithSimpleName (Just "player") ( noComponents -- the player
                        |> ECS.set playerController_ PlayerController
                        |> ECS.set position_ (Position (100, 0, 1))
                        |> ECS.set physics_ (Physics (0, 0) (Just -5000))
                        |> ECS.set spritesheet_
                            (makeSpritesheet "/assets/img/player-spritesheet.png" "running"
                                <| Dict.insert "idle"
                                    { size = (75,154)
                                    , bottomLeft = (0,(1024-154)/1024)
                                    , topRight = (1975/4096,1)
                                    , rotation = 0
                                    , pivot = (0,0)
                                    , numberOfFrames = 25
                                    , duration = 1
                                    , loop = Loop
                                    }
                                <| Dict.insert "running"
                                    { size = (132, 169)
                                    , bottomLeft = (0,(1024-154-169)/1024)
                                    , topRight = (3036/4096,(1024-154)/1024)
                                    , rotation = 0
                                    , pivot = (0,0)
                                    , numberOfFrames = 23
                                    , duration = 0.8
                                    , loop = Loop
                                    }
                                <| Dict.insert "attack"
                                    { size = (159, 190)
                                    , bottomLeft = (0,(1024-154-169-190)/1024)
                                    , topRight = (2385/4096,(1024-154-169)/1024)
                                    , rotation = 0
                                    , pivot = (0,0)
                                    , numberOfFrames = 15
                                    , duration = 0.4
                                    , loop = Loop
                                    } Dict.empty
                            ) )
        |> ECS.addEntity ( noComponents -- ground
                        |> ECS.set position_ (Position (-5000, -975, 0))
                        |> ECS.set graphic_ (Graphic 10000 1000 Color.green) )

loadTexture : String -> Cmd Msg
loadTexture filePath = Task.attempt TextureLoad <| Task.map ((,) filePath) (Texture.load filePath)

main : Program Never Model Msg
main =
    Html.program
        { init =
            ( initModel
            , Cmd.batch
                [ Task.perform WindowResize Window.size
                , loadTexture "/assets/img/temp_bg.png"
                , loadTexture "/assets/img/player-spritesheet.png"
                ]
            )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.batch
            [ AnimationFrame.diffs ((\dt -> dt / 1000) >> Tick)
            , Keyboard.downs (KeyboardEvent << Down)
            , Keyboard.ups (KeyboardEvent << Up)
            , Window.resizes WindowResize
            ]
        }

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
    case msg of
        Tick dt ->
            let newEntities =
                    model.entities
                        |> playerControlSystem model.keys
                        |> physicsSystem dt
                        |> animationSystem dt
                (Position playerPos) =
                    case Dict.get 0 newEntities of
                        Nothing -> Position (0,0,0)
                        Just player -> Maybe.withDefault (Position (0,0,0)) player.position
                cameraPos =
                    \(x, y, _) ->
                        (x*0.8, 300+y*0.2)
            in
                ({model
                    | tick = dt
                    , entities = newEntities
                    , keys = List.map (\(Key code _) -> Key code Holding) model.keys
                    , camera = Camera.moveTo (cameraPos playerPos) model.camera
                    }
                , Cmd.none)
        KeyboardEvent e ->
            ({ model | keys = keyboardSystem e model.keys }, Cmd.none)
        WindowResize size ->
            ({ model | windowSize = (size.width, size.height) }, Cmd.none)
        TextureLoad resTexture ->
            case resTexture of
                Result.Err e ->
                    let log = Debug.log "resourceError" (toString e)
                    in (model, Cmd.none)
                Result.Ok (filePath, texture) as resource ->
                    let a = Debug.log "loadedResource" (toString resource)
                    in
                        ({ model | resources = Dict.insert filePath texture model.resources }, Cmd.none)
        NoOp -> (model, Cmd.none)

view : Model -> Html.Html msg
view model =
    Html.div []
        [ renderSystem model
        -- , Html.div [] [ Html.text <| toString model.keys ]
        -- , Html.div [] [ Html.text <| toString (floor <| 1000 / model.tick) ++ " fps" ]
        -- , Html.div [] [ Html.text <| toString model.entities ]
        ]

renderSystem : Model -> Html.Html msg
renderSystem model =
    Game.renderCenteredWithOptions [] [] { time = 0, size = model.windowSize, camera = model.camera }
    << (++)
        [ Render.parallaxScroll
            { z = -0.99
            , texture = Dict.get "/assets/img/temp_bg.png" model.resources
            , tileWH = (2,2)
            , scrollSpeed = (0.005, 0.005)
            }
        , Render.parallaxScroll
            { z = -0.98
            , texture = Dict.get "/assets/img/temp_bg.png" model.resources
            , tileWH = (1,1)
            , scrollSpeed = (0.01, 0.01)
            }
        -- , case ECS.getEntityBySimpleName "player" model of
        --     Nothing -> Render.shape Render.rectangle { color = Color.yellow, position = (-1000000,0), size = (0,0) }
        --     Just entity ->
        --         Render.shapeZ Render.rectangle { color = Color.yellow, position = ECS.component (0,0,0) (\(Position p) -> p) entity.position, size=(132,169)}
        ]
    << List.filterMap (\entity ->
        let shape =
            ECS.map2 (\(Position pos) (Graphic w h color) ->
                Render.shapeZ Render.rectangle { color = color, position = pos, size = (w,h) }
            ) entity.position entity.graphic
        in
            case shape of
                Just s -> shape
                Nothing ->
                    ECS.map2
                        (\(Position pos) (Spritesheet texturePath maybeRunningAnimation _) ->
                            case maybeRunningAnimation of
                                Nothing -> Nothing
                                Just runningAnimation ->
                                    let animation = runningAnimation.currentAnimation
                                    in
                                        Just <| Render.manuallyManagedAnimatedSpriteWithOptions
                                            { texture = Dict.get texturePath model.resources
                                            , position = pos
                                            , size = animation.size
                                            , bottomLeft = animation.bottomLeft
                                            , topRight = animation.topRight
                                            , rotation = animation.rotation
                                            , pivot = animation.pivot
                                            , numberOfFrames = animation.numberOfFrames
                                            , currentFrame = runningAnimation.currentFrame
                                            }
                    ) entity.position entity.spritesheet |> Maybe.withDefault Nothing
    )
    -- Render.shapeZ doesn't seem to work, so we need to sort manually
    << List.sortBy (ECS.component 0 (\(Position (x,y,z)) -> z) << .position)
    << Dict.values
    <| model.entities

animationSystem : Time.Time -> Dict Id ComponentTable -> Dict Id ComponentTable
animationSystem dt =
    Dict.map (\_ entity ->
        entity.spritesheet
            |> Maybe.andThen getRunningAnimation
            |> Maybe.andThen (\ra ->
                let ca = ra.currentAnimation
                in
                    ECS.update spritesheet_ (\spritesheet ->
                        let setRA =
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
                            ECS.map setRA spritesheet
                ) entity |> Just
            ) |> Maybe.withDefault entity
        )

physicsSystem : Time.Time -> Dict Id ComponentTable -> Dict Id ComponentTable
physicsSystem dt =
    Dict.map (\_ entity ->
        let applyPhysics (Position (x, y, z)) (Physics (vx, vy) mGravity) =
                let vyNew = vy + (Maybe.withDefault 0 mGravity * dt)
                    xNew = vx * dt + x
                    yNew = vyNew * dt + y
                    (y_,vy_) = if yNew <= 15 then (15, 0) else (yNew, vyNew)
                in
                    [ ECS.set position_ <| Position (xNew, y_, z)
                    , ECS.set physics_ <| Physics (vx, vy_) mGravity
                    ]
        in
            case ECS.map2 applyPhysics entity.position entity.physics of
                Nothing -> entity
                Just updaters -> List.foldl (<|) entity updaters
    )

matchKey : Keyboard.KeyCode -> Key -> Bool
matchKey code (Key c _) = code == c

playerControlSystem : KeyboardInputs -> Dict Id ComponentTable -> Dict Id ComponentTable
playerControlSystem keys =
    Dict.map (\_ entity ->
        let applyControls (Position pos_) (Physics vel mGravity) PlayerController =
                { position = pos_, velocity = vel }
                |> (\p -> -- jumping
                    if V3.getY p.position <= 15 && List.member (Key 38 Pressed) keys
                        then { p | velocity = (V2.getX p.velocity, 1500) }
                        else p)
                >> (\p -> -- fast falling
                    if V3.getY p.position > 15 && List.member (Key 40 Pressed) keys
                        then { p | velocity = (V2.getX p.velocity, -1500) }
                        else p
                    )
                >> (\p -> -- horizontal movement
                    case List.head (List.filter (\(Key n _) -> n == 37 || n == 39) keys) of
                        Nothing -> { p | velocity = (0, V2.getY p.velocity) }
                        Just (Key n _) ->
                            case n of
                                37 -> { p | velocity = (-750, V2.getY p.velocity) }
                                39 -> { p | velocity = (750, V2.getY p.velocity) }
                                _ -> p
                    )
                >> (\p ->
                    [ ECS.set position_ <| Position p.position
                    , ECS.set physics_ <| Physics p.velocity mGravity
                    , ECS.update spritesheet_ <| \maybeSpritesheet ->
                        case maybeSpritesheet of
                            Nothing -> Nothing
                            Just spritesheet ->
                                if keyDown 88 keys
                                    then
                                        case getRunningAnimation spritesheet of
                                            Nothing -> Just spritesheet
                                            Just anim ->
                                                let width = (V2.getX anim.currentAnimation.size)
                                                    dir = width / (abs width)
                                                in
                                                    loadRunningAnimation "attack" spritesheet
                                                        |> mapCurrentAnimation (\cAnim ->
                                                            { cAnim
                                                                | size = (,)
                                                                    ((abs (V2.getX cAnim.size)) * dir)
                                                                    (V2.getY cAnim.size)
                                                                , pivot =
                                                                    if dir < 0 then
                                                                        (0.35,0)
                                                                    else
                                                                        (0,0)
                                                            }) |> Just
                                    else
                                        if V2.getX p.velocity == 0
                                            then
                                                case getRunningAnimation spritesheet of
                                                    Nothing -> Just (loadRunningAnimation "idle" spritesheet)
                                                    Just anim ->
                                                        let width = (V2.getX anim.currentAnimation.size)
                                                            dir = width / (abs width)
                                                        in
                                                            loadRunningAnimation "idle" spritesheet
                                                                |> mapCurrentAnimation (\cAnim ->
                                                                    { cAnim
                                                                        | size = (,)
                                                                            ((abs (V2.getX cAnim.size)) * dir)
                                                                            (V2.getY cAnim.size)
                                                                        , pivot =
                                                                            if dir < 0 then
                                                                                (0.5,0)
                                                                            else
                                                                                (0,0)
                                                                    }) |> Just
                                            else
                                                let velX = V2.getX p.velocity
                                                    dir = velX / (abs velX)
                                                    spritesheetNew = loadRunningAnimation "running" spritesheet
                                                in
                                                    case getRunningAnimation spritesheetNew of
                                                        Nothing -> Just spritesheetNew
                                                        Just anim ->
                                                            let cAnim = anim.currentAnimation
                                                            in
                                                                setRunningAnimation
                                                                    (Just { anim
                                                                        | currentAnimation =
                                                                            { cAnim
                                                                                | size = (,)
                                                                                    ((abs (V2.getX cAnim.size)) * dir)
                                                                                    (V2.getY cAnim.size)
                                                                                , pivot =
                                                                                    if dir < 1 then
                                                                                        (0.5,0)
                                                                                    else
                                                                                        (0,0)
                                                                            }
                                                                    }) spritesheetNew |> Just
                    ]
                    )
        in
            case ECS.map3 applyControls entity.position entity.physics entity.playerController of
                Nothing -> entity
                Just updaters ->
                    List.foldl (<|) entity updaters
    )

keyDown : Keyboard.KeyCode -> KeyboardInputs -> Bool
keyDown code inputs =
    case inputs of
        [] -> False
        _  -> List.any (matchKey code) inputs


keyboardSystem : KeyboardEvent -> KeyboardInputs -> KeyboardInputs
keyboardSystem e keys =
    let newKeys = List.map (\(Key code _) -> Key code Holding) keys
    in
        case e of
            Up keyCode -> List.filter (not << matchKey keyCode) newKeys
            Down keyCode ->
                if not (List.member (Key keyCode Holding) newKeys)
                    then Key keyCode Pressed :: newKeys
                    else newKeys
