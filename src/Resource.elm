module Resource exposing (..)

import Dict exposing (Dict)
import Task
import WebGL.Texture as Texture exposing (Error, Texture)

type alias FilePath = String

type alias ResourceDB o =
    { o
        | resourceLoader : Loader
    }

type alias Loader =
    { textureDB : Dict FilePath Texture
    , pending : Int
    }

type LoaderMsg = LoadTexture (Result Error (FilePath, Texture))

updateLoader : LoaderMsg -> Loader -> Loader
updateLoader msg loader =
    case msg of
        LoadTexture res ->
            case res of
                Result.Err e ->
                    let log = Debug.log "resourceError" (toString e)
                    in loader
                Result.Ok (filePath, tex) ->
                    let a = Debug.log "loadedResource" (toString tex)
                    in
                        { loader
                            | textureDB = Dict.insert filePath tex loader.textureDB
                            , pending = loader.pending - 1
                        }

loader : Loader
loader =
    { textureDB = Dict.empty
    , pending = 0
    }

initLoader : List (Cmd msg) -> (ResourceDB o, Cmd msg) -> (ResourceDB o, Cmd msg)
initLoader cmds (db, c) =
    let loader = db.resourceLoader
    in
        ( { db | resourceLoader = { loader | pending = List.length cmds } }
        , Cmd.batch (c :: cmds)
        )

loadTexture : (LoaderMsg -> msg) -> FilePath -> Cmd msg
loadTexture mkCmd filePath = Task.attempt (mkCmd << LoadTexture) <| (Texture.load filePath
    |> Task.andThen (\texture ->
        Task.succeed (filePath, texture)
    ))

saveTexture : (FilePath, Texture) -> Loader -> Loader
saveTexture (filePath, tex) loader =
    { loader | textureDB = Dict.insert filePath tex loader.textureDB  }


getTexture : FilePath -> ResourceDB o -> Maybe Texture
getTexture filePath db =
    Dict.get filePath db.resourceLoader.textureDB