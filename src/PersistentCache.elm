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


import Dict
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
type Cache data = Cache (Settings data)


type alias Settings data =
  { name : String
  , version : Int
  , maxBits : Int
  , encode : data -> Encode.Value
  , decode : Decode.Decoder data
  , policy : Policy
  , migrations : List Migration
  , overflow : Task Never ()
  }


{-|
-}
cache
  : { name : String
    , version : Int
    , kilobytes : Int
    , decode : Decode.Decoder data
    , encode : data -> Encode.Value
    }
  -> Cache data
cache { name, version, kilobytes, encode, decode } =
  Cache
    { name = name
    , version = version
    , maxBits = 8 * 1024 * kilobytes
    , decode = decode
    , encode = encode
    , policy = LRU
    , migrations = []
    , overflow = Task.succeed ()
    }



-- QUALIFIED KEYS


toQualifiedKey : Settings a -> String -> String
toQualifiedKey settings key =
  toKeyPrefix settings ++ "#" ++ key


toKeyPrefix : Settings a -> String
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
get : Cache data -> String -> Task x (Maybe data)
get (Cache settings) key =
  safely settings <| \info ->
    LS.get (toQualifiedKey settings key)
      |> andThen (getHelp settings info key)
      |> onError (\_ -> Task.succeed (Nothing, info))


getHelp
  : Settings a
  -> (Int, EQueue)
  -> String
  -> Maybe String
  -> Task LS.Error (Maybe a, (Int, EQueue))
getHelp settings info key maybeString =
  case maybeString of
    Nothing ->
      Task.succeed (Nothing, info)

    Just string ->
      let
        decoderHelp =
          Decode.object2 (,) Decode.value settings.decode
      in
        case Decode.decodeString (entryDecoder decoderHelp) string of
          Err _ ->
            -- TODO report error settings.badDecode
            Task.succeed (Nothing, info)

          Ok entry ->
            let
              (jsValue, value) =
                entry.value

              setNewValue time =
                LS.set (toQualifiedKey settings key) <|
                  Encode.encode 0 (encodeEntry (round time) jsValue)
            in
              Time.now
                |> andThen setNewValue
                |> andThen (\_ -> Task.succeed (Just value, correctInfo key info))


correctInfo : String -> (Int, EQueue) -> (Int, EQueue)
correctInfo key ((bits, equeue) as info) =
  if List.any (\item -> key == item.key) equeue then
    info

  else
    ( bits
    , List.filter (\item -> key /= item.key) equeue
    )



-- CLEAR A CACHE


{-| Clear out all the data for this particular cache.

    clear studentCache
-}
clear : Cache data -> Task x ()
clear (Cache settings) =
  let
    prefix =
      toKeyPrefix settings

    resetMetadata =
      emptyMetadata settings
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
add : Cache data -> String -> data -> Task x ()
add (Cache settings) key value =
  safely settings <| \info ->
    Time.now
      |> andThen (\time -> addHelp settings (round time) key value info)


addHelp : Settings a -> Int -> String -> a -> (Int, EQueue) -> Task x ( (), (Int, EQueue) )
addHelp settings time key value ((bits, equeue) as info) =
  let
    qualifiedKey =
      toQualifiedKey settings key

    valueString =
      Encode.encode 0 (encodeEntry time (settings.encode value))

    entryBits =
      getSize qualifiedKey valueString
  in
    if entryBits > settings.maxBits then
      LS.remove qualifiedKey
        |> onError (\_ -> Task.succeed ())
        |> andThen (\_ -> Task.succeed ( (), info ))

    else
      let
        toBitsDiff maybeOldString =
          case maybeOldString of
            Nothing ->
              entryBits

            Just oldString ->
              entryBits - getSize qualifiedKey oldString

        tryAdd bitsDiff =
          trySetWithEviction settings bitsDiff qualifiedKey (\_ _ -> valueString) bits equeue
      in
        LS.get qualifiedKey
          |> onError (\_ -> Task.succeed Nothing)
          |> Task.map toBitsDiff
          |> andThen tryAdd
          |> Task.map ((,) ())



-- GUARDED ACCESS


safely : Settings data -> ( (Int, EQueue) -> Task x ( a, (Int, EQueue) ) ) -> Task x a
safely settings doSomeStuff =
  let
    useMetadata metadata =
      checkVersion settings metadata
        |> andThen doSomeStuff
        |> andThen (adjustMetadata settings metadata)
  in
    getMetadata settings
      |> andThen useMetadata


adjustMetadata : Settings data -> Metadata -> ( a, (Int, EQueue) ) -> Task x a
adjustMetadata settings oldMetadata ( answer, (bits, equeue) ) =
  let
    allSame =
      oldMetadata.bits == bits
      && oldMetadata.equeue == equeue
      && oldMetadata.version == settings.version
  in
    if allSame then
      Task.succeed answer
    else
      let
        makeValueString currentBits currentEqueue =
          Encode.encode 0 <| encodeMetadata <|
            { version = settings.version
            , bits = currentBits
            , equeue = List.take 20 currentEqueue
            , policy = settings.policy
            }
      in
        trySetWithEviction settings 0 (toKeyPrefix settings) makeValueString bits equeue
          |> andThen (\_ -> Task.succeed answer)



-- MIGRATION


checkVersion : Settings a -> Metadata -> Task x (Int, EQueue)
checkVersion settings metadata =
  if metadata.version == settings.version then
    Task.succeed (metadata.bits, metadata.equeue)

  else
    migrate settings metadata


migrate : Settings a -> Metadata -> Task x (Int, EQueue)
migrate settings metadata =
  case findMigration metadata.version settings.version settings.migrations of
    Nothing ->
      clear (Cache settings)
        |> andThen (\_ -> Task.succeed ( 0, [] ))

    Just upgrade ->
      crawl settings (migrationStepper upgrade) Dict.empty
        |> andThen (migrateEntries settings)


type alias Keyed a =
  { key : String
  , data : a
  }


type alias Entries a =
  Dict.Dict Int (Keyed a)


migrationStepper : Upgrade -> CrawlStepper x (Entries String)
migrationStepper upgrade key oldString entries =
  case Decode.decodeString (entryDecoder Decode.value) oldString of
    Err _ ->
      LS.remove key
        |> onError (\_ -> Task.succeed ())
        |> andThen (\_ -> Task.succeed entries)

    Ok { time, value } ->
      case upgrade key value of
        Nothing ->
          LS.remove key
            |> onError (\_ -> Task.succeed ())
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


migrateEntriesHelp : Settings a -> Int -> EQueue -> List (Keyed String) -> Task x (Int, EQueue)
migrateEntriesHelp settings bits equeue entryList =
  case entryList of
    [] ->
      Task.succeed ( bits, equeue )

    { key, data } :: remainingEntries ->
      let
        entryBits =
          getSize key data

        newBits =
          bits + entryBits

        continue maybeUnit =
          case maybeUnit of
            Nothing ->
              Task.succeed ( bits, equeue )

            Just _ ->
              migrateEntriesHelp settings newBits (Keyed key entryBits :: equeue) remainingEntries
      in
        if newBits > settings.maxBits then
          Task.succeed ( bits, equeue )

        else
          LS.set key data
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
        Just (Dag.Edge from to migration)
      else
        Nothing
  in
    rawEdges
      |> List.filterMap toEdge
      |> Dag.fromList
      |> Dag.shortestPath low high
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
    |> onError (\_ -> Task.succeed [])
    |> andThen (crawlHelp (toKeyPrefix settings) stepper empty)


crawlHelp : String -> CrawlStepper x a -> a -> List String -> Task x a
crawlHelp prefix stepper acc keys =
  case keys of
    [] ->
      Task.succeed acc

    key :: remainingKeys ->
      if not (String.startsWith prefix key) then
        crawlHelp prefix stepper acc remainingKeys

      else
        LS.get key
          |> onError (\_ -> Task.succeed Nothing)
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


trySetWithEviction
  : Settings a
  -> Int
  -> String
  -> (Int -> EQueue -> String)
  -> Int
  -> EQueue
  -> Task x (Int, EQueue)
trySetWithEviction settings bitsDiff key makeValue bits equeue =
  if bits + bitsDiff > settings.maxBits then
    retrySetWithEviction settings bitsDiff key makeValue bits equeue

  else
    LS.set key (makeValue bits equeue)
      |> andThen (\_ -> Task.succeed (bits + bitsDiff, equeue))
      |> onError (\_ -> retrySetWithEviction settings bitsDiff key makeValue bits equeue)


retrySetWithEviction
  : Settings a
  -> Int
  -> String
  -> (Int -> EQueue -> String)
  -> Int
  -> EQueue
  -> Task x (Int, EQueue)
retrySetWithEviction settings bitsDiff key makeValue bits equeue =
  case equeue of
    [] ->
      flip andThen (getEvictionQueue settings) <| \newQueue ->
        if List.isEmpty newQueue then
          Task.succeed (0, [])
        else
          trySetWithEviction settings bitsDiff key makeValue bits newQueue

    {key, data} :: rest ->
      LS.remove key
        |> onError (\_ -> Task.succeed ())
        |> andThen (\_ -> trySetWithEviction settings bitsDiff key makeValue (bits - data) rest)


type alias EQueue =
  List (Keyed Int)


getEvictionQueue : Settings a -> Task x EQueue
getEvictionQueue settings =
  crawl settings evictionStepper Dict.empty
    |> Task.map Dict.values


evictionStepper : CrawlStepper x (Entries Int)
evictionStepper key valueString entries =
  case Decode.decodeString entryTimeDecoder valueString of
    Err _ ->
      LS.remove key
        |> onError (\_ -> Task.succeed ())
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


getMetadata : Settings a -> Task x Metadata
getMetadata settings =
  LS.get (toKeyPrefix settings)
    |> onError (\_ -> Task.succeed Nothing)
    |> Task.map (decodeMetadata settings)


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
