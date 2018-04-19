module ECS.Systems exposing (..)

-- reexports for convenience

import ECS.Systems.Animation
import ECS.Systems.Physics
import ECS.Systems.PlayerControl
import ECS.Systems.Render

animation = ECS.Systems.Animation.animation
physics = ECS.Systems.Physics.physics
playerControl = ECS.Systems.PlayerControl.playerControl
render = ECS.Systems.Render.render