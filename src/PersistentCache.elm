module PersistentCache exposing
  ( Cache, cache
  , get, add
  , clear
  )

{-| Cache values in a user&rsquo;s browser. If the user closes your site and comes
back tomorrow, the cached data will still be there.

> **Note:** This library is built on JavaScript&rsquo;s [`localStorage`][localStorage]
API. As of this writing, that means you get about 5mb of space. This space is
the total for your whole domain, so you cannot have two 5mb caches. Users can
also clear all this storage in their browser settings, so it is not 100%
reliable. All this is why we treat it like a cache in Elm!

[localStorage]: https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage

# Cache
@docs get, add

# Manage your Cache
@docs Cache, cache, clear

-}

{-

# Custom Caches
@docs customCache, Settings, defaultSettings

# Coordinate All Caches

When your website gets complicated, you may have a couple different caches
competing for resources. The following functions help you balance cache sizes
or just clear out everything and start fresh.

@docs summary, rebalance, clearEverything
-}


import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import String
import Task exposing (Task)
import Time

import Dag
import LocalStorage as LS



-- CACHE


{-| Description of a cache. What is it named? How big is it? What eviction
policy does it use?
-}
type Cache a = Cache (Settings a)


type alias Settings =
  { name : String
  , version : Int
  , maxBits : Int
  , encode : a -> Encode.Value
  , decode : Decode.Decoder a
  , policy : Policy
  , migrations : List Migration
  , overflow : Task Never ()
  }


{-|
-}
cache : String -> Int -> Int -> Cache
cache name version kilobytes =
  Cache
    { name = name
    , version = version
    , maxBits = 8 * 1024 * kilobytes
    , policy = LRU
    , migrations = []
    , overflow = Task.succeed ()
    }



-- QUALIFIED KEYS


toQualifiedKey : Settings a -> String -> String
toQualifiedKey settings key =
  toKeyPrefix settings ++ "#" ++ key


toKeyPrefix : Cache a -> String
toKeyPrefix { name } =
  "#" ++ name


getSize : String -> String -> Int
getSize key value =
  16 * (String.length key + String.length value)



-- GET ENTRIES


{-| Attempt to `get` some cached data. For example, if we cache some student
data to avoid HTTP requests, we may try to look it up like this:

    get studentCache "tommy"

    -- studentCache : Cache Student

This will give you Tommy&rsquo;s data as long as someone added `Student` data
for `tommy` in the past and it is still in the cache. Otherwise `Nothing`.
-}
get : Cache a -> String -> Task x (Maybe a)
get (Cache settings) key =
  LS.get (toQualifiedKey settings key)
    |> andThen (getHelp settings key)
    |> onError (\_ -> Task.succeed Nothing)


getHelp : Settings a -> String -> Maybe String -> Task LS.Error (Maybe a)
getHelp settings key maybeString =
  case maybeString of
    Nothing ->
      Task.succeed Nothing

    Just string ->
      let
        decoderHelp =
          Decode.object2 (,) Decode.value settings.decode
      in
        case Decode.decodeString (entryDecoder decoderHelp) string of
          Err _ ->
            Task.succeed Nothing

          Ok ( _, (jsValue, value) ) ->
            let
              newString =
                Encode.encode 0 (encodeEntry (round time) jsValue)
            in
              Time.now
                |> andThen (\time -> LS.set (toQualifiedKey settings key) newString)
                |> andThen (\_ -> Task.succeed (Just value))



-- CLEAR A CACHE


{-| Clear out all the data for this particular cache.

    clear studentCache
-}
clear : Cache a -> Task x ()
clear cache =
  let
    prefix =
      toKeyPrefix cache

    resetMetadata =
      emptyMetadata cache
        |> encodeMetadata
        |> Encode.encode 0
        |> LS.set prefix

    clearRelated keys =
      List.foldl (collectRemovals prefix) [resetMetadata] keys
        |> Task.sequence
        |> andThen (\_ -> Task.succeed ())
  in
    LS.keys
      |> andThen clearRelated
      |> onError (\_ -> Task.succeed ())


collectRemovals : String -> String -> List (Task LS.Error ()) -> List (Task LS.Error ())
collectRemovals prefix key removals =
  if String.startsWith prefix key then
    LS.remove key :: removals

  else
    removals



-- ADD ENTRIES


{-| Attempt to `add` some data to the cache. Maybe we just looked up
Tommy&rsquo;s student information from our servers and want to keep it around:

    add studentCache "tommy" tommy

    -- studentCache : Cache Student
    -- tommy : Student

**Note:** If the cache is full, it will evict data according to the cache
policy. If it is an LRU cache, it will kick old stuff and add Tommy. If you are
trying to add an entry that does not fit in the cache at all, it will not be
added. So if the cache holds zero kilobytes, obviously we cannot add anything.
But the more tricky case is when the cache holds 10kb, and we are trying to add
something larger than that. That is also impossible!
-}
add : Cache a -> String -> a -> Task x ()
add (Cache settings) key value =
  toKeyPrefix settings
    |> LS.get
    |> Task.map (decodeMetadata settings)
    |> andThen (checkVersion settings)
    |> andThen (addHelp settings key value)


addHelp : Settings a -> String -> a -> (Int, EQueue) -> Task x ()
addHelp settings key value (bits, equeue) =
  let
    qualifiedKey =
      toQualifiedKey settings key

    valueString =
      Encode.encode 0 (settings.encode value)

    entryBits =
      getSize qualifiedKey valueString
  in
    if entryBits > settings.maxBits then
      Task.succeed ()

    else
      trySetWithEviction settings entryBits qualifiedKey valueString bits equeue
        |> andThen (trySetMetadata settings)
        |> andThen (\_ -> Task.succeed ())


trySetMetadata : Settings a -> (Int, EQueue) -> Task x (Int, EQueue)
trySetMetadata settings (bits, equeue) =
  let
    valueString =
      Encode.encode 0 <| encodeMetadata <|
        { version = settings.version
        , bits = bits
        , equeue = equeue
        , policy = settings.policy
        }
  in
    trySetWithEviction settings 0 (toKeyPrefix settings) valueString bits equeue



-- MIGRATION


checkVersion : Settings a -> Metadata -> Task LS.Error (Int, EQueue)
checkVersion settings metadata =
  if metadata.version == settings.version then
    Task.succeed (metadata,bits, metadata.equeue)

  else
    migrate settings metadata


migrate : Settings a -> Metadata -> Task LS.Error (Int, EQueue)
migrate settings metadata =
  case findMigration metadata.version settings.version of
    Nothing ->
      clear (Cache settings)
        |> andThen (\_ -> Task.succeed ( 0, [] ))

    Just upgrade ->
      LS.keys
        |> andThen (crawl settings (migrationStepper upgrade) Dict.empty)
        |> andThen (migrateEntries settings)


type alias Keyed a =
  { key : String
  , data : a
  }


type alias Entries a =
  Dict.Dict Int (Keyed a)


migrationStepper : Upgrade -> CrawlStepper LS.Error (Entries String)
migrationStepper upgrade key oldString entries =
  case Decode.decodeString (entryDecoder Decode.value) oldString of
    Err _ ->
      LS.remove key
        |> andThen (\_ -> Task.succeed entries)

    Ok { time, value } ->
      case upgrade key value of
        Nothing ->
          LS.remove key
            |> andThen (\_ -> Task.succeed entries)

        Just newValue ->
          let
            newString =
              Encode.encode 0 (encodeEntry time newValue)
          in
            Task.succeed (Dict.insert time { key = key, data = newString } entries)


migrateEntries : Settings a -> Entries String -> Task x (Int, EQueue)
migrateEntries settings entries =
  let
    entryList =
      Dict.foldl (\_ entry list -> entry :: list) [] entries
  in
    migrateEntriesHelp settings 0 [] entryList


migrateEntriesHelp : Settings a -> Int -> EQueue -> List (String, String) -> Task x (Int, EQueue)
migrateEntriesHelp settings bits equeue entryList =
  case entryList of
    [] ->
      Task.succeed ( bits, equeue )

    (key, value) :: remainingEntries ->
      let
        entryBits =
          getSize key value

        newBits =
          bits + entryBits

        continue maybeUnit =
          case mabyeUnit of
            Nothing ->
              Task.succeed ( bits, equeue )

            Just _ ->
              migrateEntriesHelp settings newBits (Keyed key entryBits :: equeue) remainingEntries
      in
        if newBits > settings.maxBits then
          Task.succeed ( bits, equeue )

        else
          LS.set key value
            |> Task.toMaybe
            |> andThen continue



-- MIGRATION HELPERS


type alias Migration =
  { from : Int
  , to : Int
  , migration : Upgrade
  }


type alias Upgrade =
  String -> Encode.Value -> Maybe Encode.Value


findMigration : Int -> Int -> List Migration -> Maybe Upgrade
findMigration low high rawEdges =
  let
    toEdge { from, to, migration } =
      if low <= from || to <= high then
        Just (Dag.Edge from to migrations)
      else
        Nothing
  in
    rawEdges
      |> List.filterMap toEdge
      |> Dag.fromList
      |> Dag.shortestPath
      |> Maybe.map chainUpgrades


chainUpgrades : List Upgrade -> Upgrade
chainUpgrades upgrades =
  case upgrades of
    [] ->
      \_ _ -> Nothing

    first :: rest ->
     List.foldl chainUpgradesHelp first rest


chainUpgradesHelp : Upgrade -> Upgrade -> Upgrade
chainUpgradesHelp nextUpgrade upgrade =
  \key value ->
    case upgrade key value of
      Nothing ->
        Nothing

      Just nextValue ->
        nextUpgrade key nextValue



-- CRAWL EVERYTHING


type alias CrawlStepper x a =
  String -> String -> a -> Task x a


crawl : Settings data -> CrawlStepper x a -> a -> Task x a
crawl settings stepper empty =
  LS.keys
    |> andThen (crawlHelp (toKeyPrefix settings) stepper empty)


crawlHelp : Entries -> CrawlStepper x a -> a -> List String -> Task x a
crawlHelp prefix stepper acc keys =
  case keys of
    [] ->
      Task.succeed acc

    key :: remainingKeys ->
      if not (String.startsWith prefix key) then
        crawlHelp prefix stepper acc remainingKeys

      else
        LS.get key
          |> andThen (useStepper stepper acc key)
          |> andThen (\newAcc -> crawlHelp prefix stepper newAcc remainingKeys)


useStepper : CrawlStepper x a -> a -> String -> Maybe String -> Task x a
useStepper stepper acc key maybeString =
  case maybeString of
    Nothing ->
      Task.succeed acc

    Just string ->
      stepper key string acc



-- EVICTION


trySetWithEviction : Settings a -> Int -> String -> String -> Int -> EQueue -> Task x (Int, EQueue)
trySetWithEviction settings entryBits key value (bits, equeue) =
  if bits + entryBits > settings.maxBits then
    retrySetWithEviction settings entryBits key value bits equeue

  else
    LS.set key value
      |> onError (\_ -> retrySetWithEviction settings entryBits key value bits equeue)


retrySetWithEviction : Settings a -> Int -> String -> String -> Int -> EQueue -> Task x (Int, EQueue)
retrySetWithEviction settings entryBits key value bits equeue =
  case equeue of
    [] ->
      flip andThen getEvictionQueue <| \newQueue ->
        if List.isEmpty newQueue then
          Task.succeed (0, [])
        else
          trySetWithEviction settings bits newQueue entryBits key value

    {key, data} :: rest ->
      LS.remove key
        |> andThen (\_ -> trySetWithEviction settings entryBits key value (bits - data) rest)


type alias EQueue =
  List (Keyed Int)


getEvictionQueue : Settings a -> Task x EQueue
getEvictionQueue settings =
  crawl settings evictionStepper Dict.empty
    |> Task.map Dict.values
    |> onError (\_ -> [])


evictionStepper : CrawlStepper LS.Error (Entries Int)
evictionStepper key valueString entries =
  case Decode.decodeString entryTimeDecoder valueString of
    Err _ ->
      LS.remove key
        |> andThen (\_ -> Task.succeed entries)

    Ok time ->
      Task.succeed <|
        Dict.insert time (Keyed key (getSize key valueString)) entries



-- CACHE METADATA


type alias Metadata =
  { version : Int
  , bits : Int
  , equeue : EQueue
  , policy : Policy
  }


type Policy = LRU


emptyMetadata : Settings a -> Metadata
emptyMetadata { version, policy } =
  Metadata version 0 [] policy


encodeMetadata : Metadata -> Encode.Value
encodeMetadata { version, bits, equeue } =
  Encode.object
    [ ( "version", Encode.int version )
    , ( "bits", Encode.int bits )
    , ( "equeue", Encode.list (List.map encodeEvictionInfo equeue) )
    , ( "policy", Encode.string "LRU" )
    ]


metadataDecoder : Decode.Decoder Metadata
metadataDecoder =
  Decode.object4 Metadata
    ( "version" := Decode.int )
    ( "bits" := Decode.int )
    ( "equeue" := Decode.list evictionInfoDecoder )
    ( Decode.succeed LRU )


decodeMetadata : Settings a -> Maybe String -> Metadata
decodeMetadata settings maybeString =
  case Maybe.map (Decode.decodeString metadataDecoder) maybeString of
    Just (Ok metadata) ->
      metadata

    _ ->
      emptyMetadata settings



-- ENTRY METADATA


type alias Entry a =
  { time : Int
  , value : a
  }


encodeEntry : Int -> Encode.Value -> Encode.Value
encodeEntry id value =
  Encode.object
    [ ( "t", Encode.int id )
    , ( "v", value )
    ]


entryDecoder : Decode.Decoder a -> Decode.Decoder (Entry a)
entryDecoder decoder =
  Decode.object2 Entry entryTimeDecoder ( "v" := decoder )


entryTimeDecoder : Decode.Decoder Int
entryTimeDecoder =
  ( "t" := Decode.int )



-- ENCODE AND DECODE EVICTION INFO


encodeEvictionInfo : Keyed Int -> Encode.Value
encodeEvictionInfo { key, data } =
  Encode.object
    [ ( "k", Encode.string key )
    , ( "v", Encode.int data )
    ]


evictionInfoDecoder : Decode.Decoder (Keyed Int)
evictionInfoDecoder =
  Decode.object2 Keyed
    ( "k" := Decode.string )
    ( "v" := Decode.int )



-- FLIPPED TASK FUNCTIONS


andThen : (a -> Task x b) -> Task x a -> Task x b
andThen callback task =
  Task.andThen task callback


onError : (x -> Task y a) -> Task x a -> Task y a
onError callback task =
  Task.onError task callback
