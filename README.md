**NOT RELEASED YET** &mdash; I hope to release it relatively soon, but I cannot make any promises. Until then, please use ports if you want to use `localStorage`.

# Cache Data in Browsers

Cache values in a user&rsquo;s browser. If the user closes your site and comes
back tomorrow, the cached data will still be there.

> **Note:** This library is built on JavaScript&rsquo;s [`localStorage`][localStorage]
API. As of this writing, that means you get about 5mb of space. This space is
the total for your whole domain, so you cannot have two 5mb caches. Users can
also clear all this storage in their browser settings, so it is not 100%
reliable. All this is why we treat it like a cache in Elm!

[localStorage]: https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage


## Example

The [TodoMVC][] example uses the `PersistentCache` module to save the whole model. This way your todo list is there when you come back! It looks something like this:

[TodoMVC]: https://github.com/evancz/elm-todomvc

```elm
import Json.Decode as Decode
import Json.Encode as Encode
import PersistentCache as Cache

todoCache : Cache.Cache Model
todoCache =
  Cache.cache "model" 1 1024 encode decode

getModel : Task x (Maybe Model)
getModel =
  Cache.get todoCache "model"

putModel : Model -> Task x ()
putModel model =
  Cache.add todoCache "model" model
```

So we decide that we want to use at most 1mb of space on storing their `Model`. We could crank it all the way up to 5mb (the typical limit for browsers) but there still needs to be a limit. To make things more reliable, store the data on your server as well. Redundancy is a great way to reduce the risk of losing data!


## Justification

You may be wondering &ldquo;why not just give people access to the `localStorage` API directly?&rdquo;

First, **caches are better for your users.** Imagine we are storing email information for reading emails offline. Users do not want us filling up their whole device with this information, so we will want to set a limit on how much space is devoted to this. Say we choose 100kb as our limit, and after that point we start evicting the oldest info. Sounds like a pretty nice strategy: the user gets offline emails and their device does not get filled with crap!

Turns out, this is exactly how a cache works! We described an [LRU cache](https://en.wikipedia.org/wiki/Cache_algorithms) just now.

Second, **it is the right technical choice.** Even if you *want* unlimited storage in browsers, you cannot have it. Most browsers give you about 5mb of space with `localStorage`, and if you go over that quota, you start getting runtime exceptions from the JS API. So you can just hope that this never happens in practice. And when it does, you will have to find a way to migrate the data in your users&rsquo; *browsers* to a better approach. Probably some kind of cache.

Users also can clear this information from their browser settings, so even if you do not hit the limit, everything may be cleared out.

The `persistent-cache` package makes it easy to do the right thing in both cases. Storage in browsers is not reliable, and that is totally fine if you have the right mindset from the the start!
