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
import Data.Components as Component exposing (..)

addEntity : ComponentSet -> Model -> Model
addEntity components model =
    { model
        | entities = Dict.insert model.cId components model.entities
        , cId = model.cId + 1
    }

initModel : Model
initModel =
    empty
        |> addEntity ( noComponents -- the player
                        |> set playerController_ PlayerController
                        |> set position_ (Position (0, 0, 1))
                        |> set physics_ (Physics (0, 0) (Just -5000))
                        |> set graphic_ (Graphic 25 25 Color.red) )
        |> addEntity ( noComponents -- ground
                        |> set position_ (Position (-5000, -975, 0))
                        |> set graphic_ (Graphic 10000 1000 Color.green) )

main : Program Never Model Msg
main =
    Html.program
        { init =
            ( initModel
            , Cmd.batch
                [ Task.perform WindowResize Window.size
                , Task.attempt TextureLoad (Texture.load "/assets/img/temp_bg.png")
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
                (Position playerPos) =
                    case Dict.get 0 newEntities of
                        Nothing -> Position (0,0,0)
                        Just player -> Maybe.withDefault (Position (0,0,0)) player.position
                cameraPos =
                    \(x, y, _) ->
                        (x*0.9, 300+y*0.2)
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
            in ({ model | bgTexture = Result.toMaybe resTexture }, Cmd.none)
        NoOp -> (model, Cmd.none)

view : Model -> Html.Html msg
view model =
    Html.div []
        [ renderSystem model.bgTexture model.camera (Tuple.first model.windowSize) (Tuple.second model.windowSize) model.entities
        -- , Html.div [] [ Html.text <| toString model.keys ]
        -- , Html.div [] [ Html.text <| toString (floor <| 1000 / model.tick) ++ " fps" ]
        -- , Html.div [] [ Html.text <| toString model.entities ]
        ]

renderSystem : Maybe Texture -> Camera.Camera -> Int -> Int -> Dict Id ComponentSet -> Html.Html msg
renderSystem bg cam width height =
    Game.renderCenteredWithOptions [] [] { time = 0, size = (width, height), camera = cam }
    << (++)
        [ Render.parallaxScroll
            { z = -0.99
            , texture = bg
            , tileWH = (2,2)
            , scrollSpeed = (0.005, 0.005)
            }
        , Render.parallaxScroll
            { z = -0.98
            , texture = bg
            , tileWH = (1,1)
            , scrollSpeed = (0.01, 0.01)
            }
        ]
    << List.filterMap (\entity ->
        Component.map2 (\(Position pos) (Graphic w h color) ->
            Render.shapeZ Render.rectangle { color = color, position = pos, size = (w,h) }
        ) entity.position entity.graphic
    )
    -- Render.shapeZ doesn't seem to work, so we need to sort manually
    << List.sortBy (Component.component 0 (\(Position (x,y,z)) -> z) << .position)
    << Dict.values

physicsSystem : Time.Time -> Dict Id ComponentSet -> Dict Id ComponentSet
physicsSystem dt =
    Dict.map (\_ entity ->
        let applyPhysics (Position (x, y, z)) (Physics (vx, vy) mGravity) =
                let vyNew = vy + (Maybe.withDefault 0 mGravity * dt)
                    xNew = vx * dt + x
                    yNew = vyNew * dt + y
                    (y_,vy_) = if yNew <= 15 then (15, 0) else (yNew, vyNew)
                in
                    [ set position_ <| Position (xNew, y_, z)
                    , set physics_ <| Physics (vx, vy_) mGravity
                    ]
        in
            case Component.map2 applyPhysics entity.position entity.physics of
                Nothing -> entity
                Just updaters -> List.foldl (<|) entity updaters
    )

matchKey : Keyboard.KeyCode -> Key -> Bool
matchKey code (Key c _) = code == c

playerControlSystem : KeyboardInputs -> Dict Id ComponentSet -> Dict Id ComponentSet
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
                    [ set position_ <| Position p.position
                    , set physics_ <| Physics p.velocity mGravity
                    ]
                    )
        in
            case Component.map3 applyControls entity.position entity.physics entity.playerController of
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
