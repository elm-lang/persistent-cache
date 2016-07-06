module LocalStorage exposing
  ( get
  , set
  , remove
  , clear
  , keys
  , Error(..)
  )

{-|

# Storing Values
@docs get, set, remove, Error

# Curating your Storage
@docs clear, keys

-}


import Native.LocalStorage
import Task exposing (Task)


type Error = KeyNotFound String


{-| Get the value at a particular key. This can fail if the key has never been
`set` on any visit to your domain. If you would prefer to get a `Maybe` to
handle this possibility, you can make a helper like this:

    import LocalStorage
    import Task

    getMaybe : String -> Task x (Maybe String)
    getMaybe key =
      Task.toMaybe (LocalStorage.get key)
-}
get : String -> Task Error String
get =
  Native.LocalStorage.get


{-| Set a key to a particular value. If the key does not exist, it is added.
If the key already exists, we overwrite the old data.
-}
set : String -> String -> Task x ()
set =
  Native.LocalStorage.set


{-| Remove a particular key and its corresponding value.
-}
remove : String -> Task x ()
remove =
  Native.LocalStorage.remove


{-| Remove everything in local storage.
-}
clear : Task x ()
clear =
  Native.LocalStorage.clear


{-| Get all the keys currently stored. So if you `set` two entries named
`"draft"` and `"title"`, running the `keys` task will produce
`["draft", "title"]`. If you have not `set` any entries yet, you will get `[]`.
-}
keys : Task x (List String)
keys =
  Native.LocalStorage.keys

