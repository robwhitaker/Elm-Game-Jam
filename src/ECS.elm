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

processEntities : (State entity o -> Id -> entity -> (State entity o, Maybe entity, Cmd msg)) -> State entity o -> (State entity o, Cmd msg)
processEntities sysFn state =
    Dict.foldl (\id _ (s, cmds) ->
        case Dict.get id s.entities of
            Nothing -> (s, cmds)
            Just currentEntity ->
                let (newState, maybeNewEntity, newCmds) = sysFn s id currentEntity
                in
                    case maybeNewEntity of
                        Nothing -> (newState, Cmd.batch [ cmds, newCmds ])
                        Just newEntity ->
                            ( { newState | entities = Dict.insert id newEntity newState.entities }
                            , Cmd.batch [ cmds, newCmds ]
                            )
    ) (state, Cmd.none) state.entities

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
with : (entity -> Maybe a) -> (a -> value) -> entity -> Maybe value
with c f e = Maybe.map f (c e)

with2 : (entity -> Maybe a) -> (entity -> Maybe b) -> (a -> b -> value) -> entity -> Maybe value
with2 c1 c2 f e = Maybe.map2 f (c1 e) (c2 e)

with3 : (entity -> Maybe a) -> (entity -> Maybe b) -> (entity -> Maybe c) -> (a -> b -> c -> value) -> entity -> Maybe value
with3 c1 c2 c3 f e = Maybe.map3 f (c1 e) (c2 e) (c3 e)

component : b -> (a -> b) -> Maybe a -> b
component default f =
    Maybe.withDefault default << Maybe.map f