module LocalStorage exposing
  ( get
  , set
  , remove
  , clear
  , keys
  , Error(..)
  )

{-| Bindings for the [localStorage][] API. This lets you store values in a
user&rsquo;s browser. As of this writing, you get about 5mb of space that
persists from session to session. So unless they clear out their browser, the
information will be available next time they visit your site.

[localStorage]: https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage

# Storage
@docs get, set, remove, Error

# Curate your Storage
@docs clear, keys

-}


import Json.Decode as Json
import Json.Encode as Encode
import Native.LocalStorage
import Task exposing (Task)


{-| These operations can fail in a few ways.

  - `UnexpectedData` means `get` found the key, but the value was not the shape
    you wanted.
  - `QuotaExceeded` means you exceeded your 5mb (or whatever it happens to be)
    and need to `clear` or `remove` some information to make more space.
  - `Disabled` means the user turned off local storage. It is rare, but it can
  happen.

-}
type Error
  = UnexpectedData String String
  | QuotaExceeded
  | Disabled


{-| Get the value at a particular key.

    get "age" Json.Decode.int

If the key is found, `get` will try to decode the value with the decoder you
gave. If it succeeds, you get back `Just` the value you asked for. If the
decoder fails, it will trigger an `UnexpectedData` error that tells you the
key, and the error message from the decoder.

If the key is not found, you just get `Nothing` back.
-}
get : String -> Json.Decoder a -> Task Error (Maybe a)
get key decoder =
  Native.LocalStorage.get key `Task.andThen` maybeDecode key decoder


maybeDecode : String -> Json.Decoder a -> Maybe String -> Task Error (Maybe a)
maybeDecode key decoder maybeValue =
  case maybeValue of
    Nothing ->
      Task.succeed Nothing

    Just rawValue ->
      case Json.decodeString decoder rawValue of
        Ok value ->
          Task.succeed (Just value)

        Err msg ->
          Task.fail (UnexpectedData key msg)


{-| Set a key to a particular value. If the key does not exist, it is added.
If the key already exists, we overwrite the old data.

    set "age" (Json.Encode.int 42)

As of this writing, most browsers cap you at 5MB of space, so this can trigger
a `QuotaExceeded` error if you are adding enough data to cross that threshold.
-}
set : String -> Json.Value -> Task Error ()
set key value =
  Native.LocalStorage.set key (Encode.encode 0 value)


{-| Remove a particular key and its corresponding value.

    remove "age"
-}
remove : String -> Task Error ()
remove =
  Native.LocalStorage.remove


{-| Remove everything in local storage.
-}
clear : Task Error ()
clear =
  Native.LocalStorage.clear


{-| Get all the keys currently stored. So if you `set` two entries named
`"draft"` and `"title"`, running the `keys` task will produce
`["draft", "title"]`. If you have not `set` any entries yet, you will get `[]`.
-}
keys : Task Error (List String)
keys =
  Native.LocalStorage.keys

