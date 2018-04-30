module ECS.Components.AudioPlayer exposing (..)

import Set exposing (Set)
import Dict exposing (Dict)
import Array exposing (Array)

type alias FilePath = String

type AudioPlayer = AudioPlayer
    { playlist : Set String
    , playing : Set String
    , clips : Dict String AudioClip
    }

type alias AudioClip =
    { resources : Array FilePath -- if more than one resource is provided, one can be picked randomly
    , volume : Float
    , loop : Bool
    , retrigger : Bool
    }

type alias AudioPlayEvent =
    { ownerId : Int
    , label : String
    , resource : FilePath
    , volume : Float
    , loop : Bool
    }

type alias AudioStopEvent =
    { ownerId : Int
    , label : String
    }

queueAudio : String -> AudioPlayer -> AudioPlayer
queueAudio label (AudioPlayer audioPlayer) =
    AudioPlayer { audioPlayer | playlist = Set.insert label audioPlayer.playlist }

registerClip : String -> Float -> Bool -> Bool -> List FilePath -> AudioPlayer -> AudioPlayer
registerClip label volume loop retrigger resources (AudioPlayer audioPlayer) =
    AudioPlayer { audioPlayer | clips = Dict.insert label (AudioClip (Array.fromList resources) volume loop retrigger) audioPlayer.clips }