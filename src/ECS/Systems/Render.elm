module ECS.Systems.Render exposing (render)

import ECS
import ECS.Components.Simple exposing (..)
import ECS.Components.Collision exposing (..)
import ECS.Components.Spritesheet exposing (..)
import Resource
import Data.State exposing (State, System)
import Utils.SelectionList as SL

import Dict
import Html exposing (Html)
import Game.TwoD as Game
import Game.TwoD.Render as Render
import Color
import Vector2 as V2
import Vector3 as V3

render : State -> Html.Html msg
render model =
    Game.renderCenteredWithOptions [] [] { time = 0, size = model.windowSize, camera = model.camera }
    << (++)
        [ Render.parallaxScroll
            { z = -0.99
            , texture = Resource.getTexture "/assets/img/temp_bg.png" model
            , tileWH = (2,2)
            , scrollSpeed = (0.005, 0.005)
            }
        , Render.parallaxScroll
            { z = -0.98
            , texture = Resource.getTexture "/assets/img/temp_bg.png" model
            , tileWH = (1,1)
            , scrollSpeed = (0.01, 0.01)
            }
        ]
    << List.concat << List.filterMap (\entity ->
        let shape =
            Maybe.map2 (\(Position pos) (Graphic w h color) ->
                [ Render.shapeZ Render.rectangle { color = color, position = pos, size = (w,h) } ]
            ) entity.position entity.graphic
        in
            case shape of
                Just s -> shape
                Nothing ->
                    Maybe.map2
                        (\(Position pos) (Spritesheet texturePath maybeRunningAnimation _) ->
                            case maybeRunningAnimation of
                                Nothing -> Nothing
                                Just runningAnimation ->
                                    let animation = runningAnimation.currentAnimation
                                    in
                                        Just <|
                                            flip List.map (Maybe.withDefault [] (Maybe.map SL.selected animation.hitboxes))
                                                (\hbox ->
                                                    let (color, rect) =
                                                        case hbox of
                                                            Hitbox rect (Hurtbox _) -> (Color.rgba 0 0 255 0.1, rect)
                                                            Hitbox rect (Damagebox _) -> (Color.rgba 255 0 0 0.1, rect)
                                                        whbox = worldRect pos animation.size animation.pivot rect
                                                    in
                                                        Render.shapeZ Render.rectangle
                                                            { color = color
                                                            , position = whbox.position
                                                            , size = whbox.size
                                                            }
                                                )
                                            ++
                                            [ Render.manuallyManagedAnimatedSpriteWithOptions
                                                { texture = Resource.getTexture texturePath model
                                                , position = pos
                                                , size = animation.size
                                                , bottomLeft = animation.bottomLeft
                                                , topRight = animation.topRight
                                                , rotation = animation.rotation
                                                , pivot = animation.pivot
                                                , numberOfFrames = animation.numberOfFrames
                                                , currentFrame = runningAnimation.currentFrame
                                                }
                                            ]
                                            ++ case entity.hp of
                                                Nothing -> []
                                                Just (HP hp maxHP) ->
                                                    [ Render.shapeWithOptions Render.rectangle
                                                        { color = Color.black
                                                        , position = (V3.getX pos, V3.getY pos + V2.getY animation.size - 2, V3.getZ pos)
                                                        , size = (50, 11)
                                                        , rotation = 0
                                                        , pivot = (0.5, 0)
                                                        }
                                                    , Render.shapeWithOptions Render.rectangle
                                                        { color = Color.red
                                                        , position = (V3.getX pos, V3.getY pos + V2.getY animation.size, V3.getZ pos)
                                                        , size = (45*hp/maxHP, 7)
                                                        , rotation = 0
                                                        , pivot = (0.5, 0)
                                                        }
                                                    ]
                    ) entity.position entity.spritesheet |> Maybe.withDefault Nothing
    )
    -- Render.shapeZ doesn't seem to work, so we need to sort manually
    << List.sortBy (ECS.component 0 (\(Position (x,y,z)) -> z) << .position)
    << Dict.values
    <| model.entities