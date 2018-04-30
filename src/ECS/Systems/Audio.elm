module ECS.Systems.Audio exposing (audio)

import ECS
import ECS.Entity exposing (..)
import ECS.Components.AudioPlayer exposing (..)
import Data.State exposing (System)
import Ports

import Set
import Dict
import Array
import Random

audio : System msg
audio _ =
    ECS.processEntities (\state id entity ->
        entity
            |> ECS.with .audioPlayer
            |> ECS.processEntity (\(AudioPlayer audioPlayer) ->
                let (labels, cmds, randomSeed) =
                        Set.toList audioPlayer.playlist
                            |> List.foldl (\soundLabel (labels, cmds, randomSeed) ->
                                Dict.get soundLabel audioPlayer.clips
                                    |> Maybe.andThen (\audioClip ->
                                        if Set.member soundLabel audioPlayer.playing && not audioClip.retrigger
                                            then Nothing
                                            else
                                                let randSoundGen = Random.map (flip Array.get audioClip.resources) <| Random.int 0 (Array.length audioClip.resources)
                                                in
                                                    Random.step randSoundGen randomSeed
                                                        |> (\(maybeSound, newSeed) -> maybeSound
                                                        |> Maybe.andThen (\sound ->
                                                            Just
                                                                ( labels ++ [ soundLabel ]
                                                                , Cmd.batch
                                                                    [ Ports.playAudio
                                                                        { ownerId = id
                                                                        , label = soundLabel
                                                                        , resource = sound
                                                                        , volume = audioClip.volume
                                                                        , loop = audioClip.loop
                                                                        }
                                                                    , Ports.stopAudio
                                                                        { ownerId = id
                                                                        , label = soundLabel
                                                                        }
                                                                    , cmds
                                                                    ]
                                                                , newSeed
                                                                )
                                                        ))
                                    ) |> Maybe.withDefault (labels, cmds, randomSeed)
                            ) ([], Cmd.none, state.randomSeed)
                    newPlaying = Set.fromList labels |> Set.union audioPlayer.playing
                in
                    (state
                    , ECS.set audioPlayer_
                        (AudioPlayer { audioPlayer
                            | playlist = Set.empty
                            , playing = newPlaying
                        }) entity |> Just
                    , cmds
                    )
            ) |> Maybe.withDefault (state, Nothing, Cmd.none)
    )