# `abstract-fork`

A type class `MonadFork` that allows you to provide an alternative behavior for forking threads.

## Motivation

Consider the `resourcet` package. It defines a custom [`resourceForkIO`](https://hackage.haskell.org/package/resourcet-1.3.0/docs/Control-Monad-Trans-Resource.html#v:resourceForkIO), with the following documentation:

> Launch a new reference counted resource context using forkIO.
> 
> This is defined as resourceForkWith forkIO.
> 
> Note: Using regular forkIO inside of a ResourceT is inherently unsafe, since the forked thread may try access the resources of the parent after they are cleaned up. When you use resourceForkIO or resourceForkWith, ResourceT is made aware of the new thread, and will only cleanup resources when all threads finish. Other concurrency mechanisms, like concurrently or race, are safe to use.
> 
> If you encounter InvalidAccess exceptions ("The mutable state is being accessed after cleanup"), use of forkIO is a possible culprit.

`resourceForkWith` has more information:

> Introduce a reference-counting scheme to allow a resource context to be shared by multiple threads. Once the last thread exits, all remaining resources will be released.
> 
> The first parameter is a function which will be used to create the thread, such as forkIO or async.
> 
> Note that abuse of this function will greatly delay the deallocation of registered resources. This function should be used with care. A general guideline:
> 
> If you are allocating a resource that should be shared by multiple threads, and will be held for a long time, you should allocate it at the beginning of a new ResourceT block and then call resourceForkWith from there.

This is used with the `yesod` package to fork threads with [`forkHandler`](https://hackage.haskell.org/package/yesod-core-1.6.24.0/docs/Yesod-Core-Handler.html#v:forkHandler).
If you're using `yesod` and doing a `fork` without knowing about `forkHandler`, then you may be exposing yourself to resource bugs.

Consider [`hs-opentelemetry`](https://github.com/iand675/hs-opentelemetry#readme).
This library uses a thread-local context to provide transparent instrumentation libraries and detailed span information.
However, if you want that context to be in a forked thread, you need to redefine `fork` for yourself:

```haskell
fork :: (MonadUnliftIO m) => m () -> m ThreadId
fork action = 
    withRunInIO $ \runInIO -> do
        context <- getContext
        Control.Concurrent.forkIO $ do
            attachContext context
            runInIO action
```

However, this *won't* get used unless you remember to call it, and it won't get used by any libraries.
So you'll need to redefine your own concurrency functions - including special variants of `async`.

This library provides an alternative - `MonadFork`.
`MonadFork` allows you to say *how* your app will fork threads.
