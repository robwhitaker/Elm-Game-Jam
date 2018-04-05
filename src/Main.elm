module Main exposing (main)

import Html
import Collage
import Element
import Time

import AnimationFrame
import Keyboard

import Data.Model exposing (..)
import Messages exposing (..)
import Data.Types exposing (..)
import Data.Components exposing (..)

main : Program Never Model Msg
main =
    Html.program
        { init = (empty, Cmd.none)
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
            ]

renderSystem : Float -> Float -> List (List Component) -> Element.Element
renderSystem width height =
    let drawAt = \w h (x, y) -> Collage.move (-width/2+w/2+x, -height/2+h/2+y) --set origin to bottom left
    in
        Collage.collage (floor width) (floor height)
        << List.map Tuple.second
        << List.sortBy Tuple.first
        << List.filterMap (\entity ->
            Maybe.map2 (\p g ->
                case (p, g) of
                    ((Position pos), (Graphic w h color layer)) ->
                        (layer, Collage.rect w h |> Collage.filled color |> drawAt w h pos)
                    _ -> Debug.crash "renderSystem: Bad pattern match."
            ) (getComponent position entity) (getComponent graphic entity)
        )

physicsSystem : Time.Time -> List (List Component) -> List (List Component)
physicsSystem dt =
    List.map (\entity ->
        let applyPhysics pos phys =
                case (pos, phys) of
                    (Position (x, y), Physics (vx, vy) mGravity) ->
                        let vyNew = vy + (Maybe.withDefault 0 mGravity * Time.inSeconds dt)
                            xNew = vx * (Time.inSeconds dt) + x
                            yNew = vyNew * (Time.inSeconds dt) + y
                            (y_,vy_) = if yNew <= 15 then (15, 0) else (yNew, vyNew)
                        in
                            [ updateComponent position (Position (xNew, y_))
                            , updateComponent physics (Physics (vx, vy_) mGravity)
                            ]
                    _ -> Debug.crash "physicsSystem: Bad pattern match."
        in
            case Maybe.map2 applyPhysics (getComponent position entity) (getComponent physics entity) of
                Nothing -> entity
                Just updaters -> List.foldl (<|) entity updaters
    )

matchKey : Keyboard.KeyCode -> Key -> Bool
matchKey code (Key c _) = code == c

playerControlSystem : KeyboardInputs -> List (List Component) -> List (List Component)
playerControlSystem keys =
    List.map (\entity ->
        let applyControls pos phys _ =
            case (pos, phys) of
                (Position pos_, Physics vel mGravity) ->
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
                        [ updateComponent position (Position p.position)
                        , updateComponent physics (Physics p.velocity mGravity)
                        ]
                        )
                _ -> Debug.crash "playerControlSystem: Bad pattern match."
        in
            case Maybe.map3 applyControls (getComponent position entity) (getComponent physics entity) (getComponent playerController entity) of
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
