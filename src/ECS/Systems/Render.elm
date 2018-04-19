module ECS.Systems.Render exposing (render)

import ECS
import ECS.Components.Simple exposing (..)
import ECS.Components.Spritesheet exposing (..)
import Resource
import Data.State exposing (State, System)

import Dict
import Html exposing (Html)
import Game.TwoD as Game
import Game.TwoD.Render as Render

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
                    ) entity.position entity.spritesheet |> Maybe.withDefault Nothing
    )
    -- Render.shapeZ doesn't seem to work, so we need to sort manually
    << List.sortBy (ECS.component 0 (\(Position (x,y,z)) -> z) << .position)
    << Dict.values
    <| model.entities