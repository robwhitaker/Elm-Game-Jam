module Main exposing (main)

import Html
import Collage
import Element
import Text
import Color
import Time

import AnimationFrame
import Keyboard

import Data.Model exposing (..)
import Messages exposing (..)
import Data.Types exposing (..)

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
    let gravity = -5000
    in
        case msg of
            Tick dt ->
                let vy = Tuple.second model.player.velocity + gravity * Time.inSeconds dt
                    x = (Tuple.first model.player.velocity) * (Time.inSeconds dt) + Tuple.first model.player.position
                    y = vy * (Time.inSeconds dt) + Tuple.second model.player.position
                    (y_,vy_) = if y <= 15 then (15,0) else (y,vy)
                    player = model.player
                in
                    ({model
                        | tick = Time.inSeconds dt
                        , player = { player | position = (x,y_), velocity = (Tuple.first player.velocity, vy_) }
                        }
                    , Cmd.none)
            KeyboardEvent e ->
                let m = { model | keys = keyboardSystem e model.keys }
                in
                    case e of
                        Down 38 -> let player = m.player in ({m | player = { player | velocity = (Tuple.first player.velocity, 1500) }}, Cmd.none)
                        Down 37 -> let player = m.player in ({m | player = { player | velocity = (-1000, Tuple.second player.velocity) }}, Cmd.none)
                        Down 39 -> let player = m.player in ({m | player = { player | velocity = (1000, Tuple.second player.velocity) }}, Cmd.none)
                        Down 40 -> let player = m.player in ({m | player = { player | velocity = (Tuple.first player.velocity, -1500) }}, Cmd.none)
                        Up   37 -> let player = m.player in ({m | player = { player | velocity = (0, Tuple.second player.velocity) }}, Cmd.none)
                        Up   39 -> let player = m.player in ({m | player = { player | velocity = (0, Tuple.second player.velocity) }}, Cmd.none)
                        _ -> (m, Cmd.none)
            NoOp -> (model, Cmd.none)

view : Model -> Html.Html msg
view model =
    let width = 1000
        height = 500
        drawAt = originBottomLeft width height
    in
        Html.div []
            [ Element.toHtml <| Collage.collage width height
                [ Collage.rect width 15 |> Collage.filled Color.green |> drawAt width 15 (0,0)
                , Collage.rect 25 25 |> Collage.filled Color.red |> drawAt 25 25 model.player.position
                , Text.fromString (toString model.tick) |> Collage.text |> drawAt 15 15 (width-15, height-15)
                ]
            , Html.text <| toString model.keys
            ]

originBottomLeft : Float -> Float -> (Float -> Float -> (Float, Float) -> Collage.Form -> Collage.Form)
originBottomLeft width height =
    \w h (x, y) -> Collage.move (-width/2+w/2+x, -height/2+h/2+y)

matchKey : Keyboard.KeyCode -> Key -> Bool
matchKey code (Key c _) = code == c

keyboardSystem : KeyboardEvent -> KeyboardInputs -> KeyboardInputs
keyboardSystem e keys =
    let newKeys = List.map (\(Key code _) -> Key code Old) keys
    in
        case e of
            Up keyCode -> List.filter (not << matchKey keyCode) newKeys
            Down keyCode ->
                if not (List.member (Key keyCode Old) newKeys)
                    then Key keyCode New :: newKeys
                    else newKeys
