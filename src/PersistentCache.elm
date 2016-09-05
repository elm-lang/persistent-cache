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
import LocalStorage as LS
import String
import Task exposing (Task)
import Time



-- CACHE


{-| Description of a cache. What is it named? How big is it? What eviction
policy does it use?
-}
type Cache = Cache Settings


type alias Settings =
  { name : String
  , version : Int
  , maxBits : Int
  , policy : Policy
  , migrate : MigrationStrategy
  , overflow : Task Never ()
  }


type MigrationStrategy
  = GiveUp
  | Try (Int -> String -> Encode.Value -> Maybe Encode.Value)


{-|
-}
cache : String -> Int -> Int -> Cache
cache name version kilobytes =
  Cache
    { name = name
    , version = version
    , maxBits = 8 * 1024 * kilobytes
    , policy = LRU
    , migrate = GiveUp
    , overflow = Task.succeed ()
    }



-- QUALIFIED KEYS


toQualifiedKey : Cache -> String -> String
toQualifiedKey cache key =
  toKeyPrefix cache ++ "#" ++ key


toKeyPrefix : Cache -> String
toKeyPrefix (Cache { name, version }) =
  name ++ "#" ++ toString version



-- GET ENTRIES


{-| Attempt to `get` some cached data. For example, if we cache some student
data to avoid HTTP requests, we may try to look it up like this:

    get studentCache "tommy" studentDecoder

    -- studentCache : Cache
    -- studentDecoder : Json.Decoder Student

This will give you Tommy&rsquo;s data as long as someone added `Student` data
for `tommy` in the past and it is still in the cache. Otherwise `Nothing`.
-}
get : Cache -> String -> Decode.Decoder a -> Task x (Maybe a)
get cache key decoder =
  let
    qualifiedKey =
      toQualifiedKey cache key
  in
    LS.get qualifiedKey
      |> andThen (getHelp qualifiedKey decoder)
      |> onError (\_ -> Task.succeed Nothing)


getHelp : String -> Decode.Decoder a -> Maybe String -> Task LS.Error (Maybe a)
getHelp qualifiedKey decoder maybeString =
  case maybeString of
    Nothing ->
      Task.succeed Nothing

    Just string ->
      let
        decoderHelp =
          Decode.object2 (,) Decode.value decoder
      in
        case Decode.decodeString (entryDecoder decoderHelp) string of
          Err _ ->
            Task.succeed Nothing

          Ok (jsValue, value) ->
            Time.now
              |> andThen (\time -> LS.set qualifiedKey (Encode.encode 0 (encodeEntry (round time) jsValue)))
              |> andThen (\_ -> Task.succeed (Just value))



-- CLEAR A CACHE


{-| Clear out all the data for this particular cache.

    clear studentCache
-}
clear : Cache -> Task x ()
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

    add studentCache "tommy" (encode tommy)

    -- studentCache : Cache
    -- encode : Student -> Json.Value
    -- tommy : Student

**Note:** If the cache is full, it will evict data according to the cache
policy. If it is an LRU cache, it will kick old stuff and add Tommy. If you are
trying to add an entry that does not fit in the cache at all, it will not be
added. So if the cache holds zero kilobytes, obviously we cannot add anything.
But the more tricky case is when the cache holds 10kb, and we are trying to add
something larger than that. That is also impossible!
-}
add : Cache -> String -> Encode.Value -> Task x ()
add (Cache settings as cache) key value =
  let
    prefix =
      toKeyPrefix cache

    qualifiedKey =
      toQualifiedKey cache key

    valueString =
      Encode.encode 0 value

    entryBits =
      16 * (String.length qualifiedKey + String.length valueString)

    checkVersion metadata =
      iff (metadata.version /= settings.version) (migrate cache) metadata

    checkSize metadata =
      iff (metadata.bits + entryBits > settings.maxBits) (evict cache entryBits) metadata
  in
    if entryBits > settings.maxBits then
      Task.succeed ()

    else
      Task.map (getMetadata cache) (LS.get prefix)
        |> andThen checkVersion
        |> andThen checkSize
        |> andThen (addEntry prefix qualifiedKey valueString)
        |> onError (\_ -> Debug.crash "TODO")


getMetadata : Cache -> Maybe String -> Metadata
getMetadata cache maybeString =
  case Maybe.map (Decode.decodeString metadataDecoder) maybeString of
    Just (Ok metadata) ->
      metadata

    _ ->
      emptyMetadata cache


iff : Bool -> (a -> Task x a) -> a -> Task x a
iff condition func value =
  if condition then func value else Task.succeed value


migrate : Cache -> Metadata -> Task LS.Error Metadata
migrate cache metadata =
  Debug.crash "TODO"


evict : Cache -> Int -> Metadata -> Task LS.Error Metadata
evict cache entryBits metadata =
  Debug.crash "TODO"


addEntry : String -> String -> String -> Metadata -> Task LS.Error ()
addEntry prefix qualifiedKey valueString metadata =
  Debug.crash "TODO"



-- CACHE METADATA


type alias Metadata =
  { version : Int
  , bits : Int
  , candidates : List String
  , policy : Policy
  }


type Policy = LRU


emptyMetadata : Cache -> Metadata
emptyMetadata (Cache { version, policy }) =
  Metadata version 0 [] policy


encodeMetadata : Metadata -> Encode.Value
encodeMetadata { version, bits, candidates } =
  Encode.object
    [ ( "version", Encode.int version )
    , ( "bits", Encode.int bits )
    , ( "candidates", Encode.list (List.map Encode.string candidates) )
    , ( "policy", Encode.string "LRU" )
    ]


metadataDecoder : Decode.Decoder Metadata
metadataDecoder =
  Decode.object4 Metadata
    ( "version" := Decode.int )
    ( "bits" := Decode.int )
    ( "candidates" := Decode.list Decode.string )
    ( Decode.succeed LRU )



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


entryDecoder : Decode.Decoder a -> Decode.Decoder a
entryDecoder decoder =
  ( "v" := decoder )



-- FLIPPED TASK FUNCTIONS


andThen : (a -> Task x b) -> Task x a -> Task x b
andThen callback task =
  Task.andThen task callback


onError : (x -> Task y a) -> Task x a -> Task y a
onError callback task =
  Task.onError task callback
