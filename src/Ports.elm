port module Ports exposing (..)

import ECS.Components.AudioPlayer exposing (AudioClip, AudioPlayEvent, AudioStopEvent)


port playAudio : AudioPlayEvent -> Cmd msg

port stopAudio : AudioStopEvent -> Cmd msg

-- (audio filepath, list of extensions)
port loadAudio : (String, List String) -> Cmd msg

-- audio filepath to msg
port audioLoaded : (String -> msg) -> Sub msg

-- (entity id, audio label)
port audioEnded : ((Int, String) -> msg) -> Sub msg

-- (entity id, audio label)
port audioStopped : ((Int, String) -> msg) -> Sub msg