module Main exposing (main)

import Html
import Collage
import Element
import Time
import Color
import Dict exposing (Dict)

import AnimationFrame
import Keyboard

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
                        |> set position_ (Position (0, 0))
                        |> set physics_ (Physics (0, 0) (Just -5000))
                        |> set graphic_ (Graphic 25 25 Color.red 1) )
        |> addEntity ( noComponents -- ground
                        |> set position_ (Position (0, 0))
                        |> set graphic_ (Graphic 1000 30 Color.green -1) )

main : Program Never Model Msg
main =
    Html.program
        { init = (initModel, Cmd.none)
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.batch
            [ AnimationFrame.diffs Tick
            , Keyboard.downs (KeyboardEvent << Down)
            , Keyboard.ups (KeyboardEvent << Up)
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
            in
                ({model
                    | tick = dt
                    , entities = newEntities
                    , keys = List.map (\(Key code _) -> Key code Holding) model.keys
                    }
                , Cmd.none)
        KeyboardEvent e ->
            ({ model | keys = keyboardSystem e model.keys }, Cmd.none)
        NoOp -> (model, Cmd.none)

view : Model -> Html.Html msg
view model =
    let width = 1000
        height = 500
    in
        Html.div []
            [ Element.toHtml <| renderSystem width height model.entities
            , Html.div [] [ Html.text <| toString model.keys ]
            , Html.div [] [ Html.text <| toString (floor <| 1000 / model.tick) ++ " fps" ]
            , Html.div [] [ Html.text <| toString model.entities ]
            ]

renderSystem : Float -> Float -> Dict Id ComponentSet -> Element.Element
renderSystem width height =
    let drawAt = \w h (x, y) -> Collage.move (-width/2+w/2+x, -height/2+h/2+y) --set origin to bottom left
    in
        Collage.collage (floor width) (floor height)
        << List.map Tuple.second
        << List.sortBy Tuple.first
        << List.filterMap (\entity ->
            Component.map2 (\(Position pos) (Graphic w h color layer) ->
                (layer, Collage.rect w h |> Collage.filled color |> drawAt w h pos)
            ) entity.position entity.graphic
        )
        << Dict.values

physicsSystem : Time.Time -> Dict Id ComponentSet -> Dict Id ComponentSet
physicsSystem dt =
    Dict.map (\_ entity ->
        let applyPhysics (Position (x, y)) (Physics (vx, vy) mGravity) =
                let vyNew = vy + (Maybe.withDefault 0 mGravity * Time.inSeconds dt)
                    xNew = vx * (Time.inSeconds dt) + x
                    yNew = vyNew * (Time.inSeconds dt) + y
                    (y_,vy_) = if yNew <= 15 then (15, 0) else (yNew, vyNew)
                in
                    [ set position_ <| Position (xNew, y_)
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
                    if Tuple.second p.position <= 15 && List.member (Key 38 Pressed) keys
                        then { p | velocity = (Tuple.first p.velocity, 1500) }
                        else p)
                >> (\p -> -- fast falling
                    if Tuple.second p.position > 15 && List.member (Key 40 Pressed) keys
                        then { p | velocity = (Tuple.first p.velocity, -1500) }
                        else p
                    )
                >> (\p -> -- horizontal movement
                    case List.head (List.filter (\(Key n _) -> n == 37 || n == 39) keys) of
                        Nothing -> { p | velocity = (0, Tuple.second p.velocity) }
                        Just (Key n _) ->
                            case n of
                                37 -> { p | velocity = (-750, Tuple.second p.velocity) }
                                39 -> { p | velocity = (750, Tuple.second p.velocity) }
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
