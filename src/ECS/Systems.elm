module ECS.Systems exposing (..)

-- reexports for convenience

import ECS.Systems.Animation
import ECS.Systems.Physics
import ECS.Systems.PlayerControl
import ECS.Systems.Render
import ECS.Systems.Collision
import ECS.Systems.AI
import ECS.Systems.Audio

animation = ECS.Systems.Animation.animation
physics = ECS.Systems.Physics.physics
playerControl = ECS.Systems.PlayerControl.playerControl
render = ECS.Systems.Render.render
collision = ECS.Systems.Collision.collision
ai = ECS.Systems.AI.ai
audio = ECS.Systems.Audio.audio