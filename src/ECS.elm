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

-- remove an entity by its id. Does not currently clean up the simple name.
removeEntityById : Id -> State entity o -> State entity o
removeEntityById id state =
    { state | entities = Dict.remove id state.entities }

-- update an entity by its id
updateEntityById : Id -> (entity -> entity) -> State entity o -> State entity o
updateEntityById id fn state =
    { state | entities = Dict.update id (Maybe.map fn) state.entities }

-- update an entity by its simple name
updateEntityBySimpleName : String -> (entity -> entity) -> State entity o -> State entity o
updateEntityBySimpleName name fn state =
    Dict.get name state.entitySimpleNames
        |> Maybe.andThen (\id -> Just <| updateEntityById id fn state)
        |> Maybe.withDefault state

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

-- Helpers for matching entities

-- Continuation-ish style. Can be used when more than the withN functions provide is needed.
with : (entity -> Maybe a) -> entity -> Maybe (entity, ((a -> r) -> r))
with getter entity =
    Maybe.map ((,) entity << (|>)) (getter entity)

andWith : (entity -> Maybe b) -> Maybe (entity, a -> b -> r) -> Maybe (entity, a -> r)
andWith getter =
    Maybe.andThen (\(entity, cont) ->
        Maybe.map (\component -> (,) entity <| \c -> ((|>) component) (cont c)) (getter entity))

processEntity : a -> Maybe (entity, (a -> r)) -> Maybe r
processEntity f maybeCont =
    Maybe.map ((|>) f << Tuple.second) maybeCont

-- Misc. Helpers

component : b -> (a -> b) -> Maybe a -> b
component default f =
    Maybe.withDefault default << Maybe.map f