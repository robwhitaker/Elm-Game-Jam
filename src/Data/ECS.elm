module Data.ECS exposing (..)

import Dict exposing (Dict)

------ CORE ------

type alias Id = Int

type alias State entity o =
    { o | entities : Dict Id entity
        , entitySimpleNames : Dict String Id
        , cId : Int
    }

------ ENTITY ------

-- add an entity without a simple name
addEntity : entity -> State entity o -> State entity o
addEntity = addEntityWithSimpleName Nothing

-- add an entity, optionally with a simple name
addEntityWithSimpleName : Maybe String -> entity -> State entity o -> State entity o
addEntityWithSimpleName maybeSimpleName entity state =
    { state
        | entities = Dict.insert state.cId entity state.entities
        , entitySimpleNames =
            case maybeSimpleName of
                Nothing  -> state.entitySimpleNames
                Just str -> Dict.insert str state.cId state.entitySimpleNames
        , cId = state.cId + 1
    }

-- get an entity by id
getEntityById : Id -> State entity o -> Maybe entity
getEntityById id state =
    Dict.get id state.entities

-- get an entity by its simple name
getEntityBySimpleName : String -> State entity o -> Maybe entity
getEntityBySimpleName name state =
    Dict.get name state.entitySimpleNames
        |> Maybe.andThen (flip getEntityById state)

------ SYSTEM ------

type alias System entity o msg =
    (State entity o, Cmd msg) -> (State entity o, Cmd msg)

------ COMPONENT ------

-- Updater helpers
type alias Updater a componentTable = (Maybe a -> Maybe a) -> componentTable -> componentTable

set : Updater a componentTable -> a -> componentTable -> componentTable
set updater = updater << always << Just

update : Updater a componentTable -> Updater a componentTable
update = identity

remove : Updater a componentTable -> componentTable -> componentTable
remove updater = updater (always Nothing)

-- Mapping functions (re-exports from Maybe, in case of implementation change)
map : (a -> b) -> Maybe a -> Maybe b
map = Maybe.map

map2 : (a -> b -> value) -> Maybe a -> Maybe b -> Maybe value
map2 = Maybe.map2

map3 : (a -> b -> c -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe value
map3 = Maybe.map3

component : b -> (a -> b) -> Maybe a -> b
component default f =
    Maybe.withDefault default << map f