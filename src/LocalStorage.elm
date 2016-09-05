module LocalStorage exposing
  ( get
  , set
  , remove
  , clear
  , keys
  , Error(..)
  )

{-| Low-level bindings to the [localStorage][] API.

[localStorage]: https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage

# Storage
@docs get, set, remove, Error

# Curate your Storage
@docs clear, keys

-}


import Native.LocalStorage
import Task exposing (Task)


{-| These low-level operations can fail in a few ways:

  - `QuotaExceeded` means you exceeded your 5mb and need to `clear` or `remove`
    some information to make more space.
  - `Disabled` means the user turned off local storage. It is rare, but it can
    happen.
-}
type Error
  = QuotaExceeded
  | Disabled


{-| Get the value at a particular key.

    get "age"
-}
get : String -> Task Error (Maybe String)
get =
  Native.LocalStorage.get


{-| Set a key to a particular value. If the key does not exist, it is added.
If the key already exists, we overwrite the old data.

    set "age" "42"

Most browsers cap you at 5MB of space, so this can trigger a `QuotaExceeded`
error if you are adding enough data to cross that threshold.
-}
set : String -> String -> Task Error ()
set =
  Native.LocalStorage.set


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
