module ECS exposing (..)

import Dict exposing (Dict)
import Time exposing (Time)

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
    Time -> State entity o -> (State entity o, Cmd msg)

runSystems : Time -> State entity o -> List (System entity o msg) -> (State entity o, Cmd msg)
runSystems dt state systems =
    List.foldl (\system (state, cmds) ->
        let (newState, newCmds) = system dt state
        in
            (newState, Cmd.batch [newCmds, cmds])
    ) (state, Cmd.none) systems

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

map4 : (a -> b -> c -> d -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe value
map4 = Maybe.map4

component : b -> (a -> b) -> Maybe a -> b
component default f =
    Maybe.withDefault default << map f