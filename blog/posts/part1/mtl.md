---
title: Adding `mtl`
published: 2015-09-14 12:00:00+10:00
---

# Introduction

[Previously](./cycle.html) we expanded on the example from `sdl2`.

In this post we'll be adding monad transformers to the code.

We're doing this to reduce the cost of change for the code, as well as to remove the need to manually plumb configuration and state through our code.
For now the reduced plumbing will be the most obvious benefit.

The code is available [here](https://github.com/dalaing/falling-blocks/tree/master/code/part1/mtl).

## What you need to know about monad transformers and `mtl` for now

### Effects

In this post we'll be working with `Reader`, `State` and `IO` effects.

I'm assuming there's a good chance that you're familiar with these.
If you're not familiar with `IO`, you should probably read a bit more about monads and `IO` and come back later.

The `Reader` effect is for passing configuration information around.
We can use `ReaderT` to do this while other effects are in play.

We can go from this:
```haskell
foo, must, die :: Config -> IO ()

foo x = do
  must x
  die x
```
to this:
```haskell
foo, must, die :: ReaderT Config IO ()

foo = do
  must
  die
```

If we need to access to the configuration, we can use `ask`:
```haskell
ask :: ReaderT r m -> m r
```
like so:
```haskell
must :: ReaderT Config IO ()
must = do
  ...
  c <- ask
  putStrLn c
  ...
```

As we use the configuration in more places and inside functions with more levels of nesting, we increase the amount of plumbing that gets saved.

We use `runReaderT`:
```haskell
runReaderT :: ReaderT r m a -> r -> m a
```
to actually make use of this, which in this case would work something like:
```haskell
runReaderT foo exampleConfig :: IO ()
```

The `State` effect is for threading state through computations, and there is also a `StateT` for when other effects are in play.

We can go from this:
```haskell
foo, must, die :: MyState -> IO MyState

foo s = do
  s' <- must s
  s'' <- die s'
  return s''
```
to this:
```haskell
foo, must, die :: StateT MyState IO ()

foo = do
  must
  die
```

We can use `get`, `put` and `modify` to work with the `State` while we're working inside `State` or `StateT`.

Instead of `runReaderT` we have `runStateT`, `evalStateT` and `execStateT`:
```haskell
runStateT :: StateT s m a -> s -> m (a, s)
evalStateT :: StateT s m a -> s -> m a
execStateT :: StateT s m a -> s -> m s
```
depending on what we want as output.

### Combining effects

Monad transformers allow us to combine effects.

We'll mostly be dealing with:
```haskell
ReaderT Renderer (StateT GameState IO) a
```

The order matters for monad transformers, so this is a different type than:
```haskell
StateT GameState (ReaderT Renderer IO) a
```
but it won't effect us too much.

The monad transformers themselves have `Monad` instances:
```haskell
instance Monad m => Monad (StateT m)
instance Monad m => Monad (ReaderT m)
```
which is what allows us to use them with do-notation.

There are 3 effects in these monad transformer stacks, and we can work with these effects via a number of typeclasses provided by `mtl`:

- `MonadReader r m` lets us work with `Reader` effects
- `MonadState s m` lets us work with `State` effects
- `MonadIO m` lets us work with `IO` effects

These will work no matter where the effect is in our stack, or what other effects are in the stack.

I like that a great deal, since it means I can get some code written quickly and nail down the details of the stack later on.

That should be enough to get started, so let us dig in.

## Updating the event handling 

If we look at the current type signature of `handleEvent`:
```haskell
handleEvent :: AppEvent
            -> GameState
            -> GameState
```
we see that the only effect we're after is managing state of type `GameState`.

We can express this like so:
```haskell
handleEvent :: MonadState GameState m
            => AppEvent
            -> m ()
```

The change to the code is minor.

We start with this:
```haskell
handleEvent :: AppEvent
            -> GameState
            -> GameState
handleEvent AppCycle gs = gs { backgroundColour = nextColour . backgroundColour $ gs }
handleEvent AppQuit  gs = gs { hasQuit = True }
handleEvent _        gs = gs
```
which is updating the state using a function of type `GameState -> GameState`.

That is exactly what `modify` does:
```haskell
modify :: MonadState s m => (s -> s) -> m ()
```
so we use that:
```haskell
handleEvent :: MonadState GameState m
            => AppEvent
            -> m ()
handleEvent AppCycle = modify $ \gs -> gs { backgroundColour = nextColour . backgroundColour $ gs }
handleEvent AppQuit  = modify $ \gs -> gs { hasQuit = True }
handleEvent _        = return ()
```

We also need to update `doEvents`:
```haskell
doEvents :: GameState
         -> IO GameState
```
which is managing `IO` effects as well as managing state of type `GameState`.

We can bring the `IO` into an `mtl` style typeclass constraint with `MonadIO m`:
```haskell
doEvents :: MonadIO m
         => GameState
         -> m GameState
```
after which we can add the `MonadState GameState m` constraint:
```haskell
doEvents :: (MonadState GameState m, MonadIO m)
         => m ()
```

We'll update the code slowly for this one.

We start with this:
```haskell
doEvents :: GameState
         -> IO GameState
doEvents gs = do
  es <- pollEvents
  return $ foldr (handleEvent . toAppEvent) gs es
```

We can use `liftIO` to lift a concrete `IO` action to the more abstract `MonadIO m` action:
```haskell
liftIO :: MonadIO m => IO a -> m a
```
which gets us to the second iteration of `doEvents`:
```haskell
doEvents :: MonadIO m
         => GameState
         -> m GameState
doEvents gs = do
  es <- liftIO pollEvents
  return $ foldr (handleEvent . toAppEvent) gs es
```

We now need to deal with the new `handleEvent`, which operates with `MonadState GameState m` in the context.

We can use `traverse_` from `Data.Foldable` in the `base` package to do this.

The type signature is:
```haskell
traverse_ :: (Foldable t, Applicative f)
          => (a -> f b)
          -> t a
          -> f ()
```

There is a `Foldable` instance for `List`, so we can specialize to:
```haskell
traverse_ :: Applicative f
          => (a -> f b)
          -> [a]
          -> f ()
```

We can partly specialize the `Applicative` constraint.
With `MonadState GameState m` in context, `m` is a `Monad`, and every `Monad` is an `Applicative`.

This gives us:
``haskell
traverse_ :: MonadState GameState m
          => (a -> m b)
          -> [a]
          -> m ()
```

If we use `AppEvent` for `a` and `()` for `b` we get as far as we can for `traverse_`:
```haskell
traverse_ :: MonadState GameState m
          => (AppEvent -> m ())
          -> [AppEvent]
          -> m ()
```

We already have a function with the right type for the first argument:
```haskell
handleEvent :: MonadState GameState m
            => AppEvent
            -> m ()
```
but the list of events we get from `pollEvents` has type `[Event]`.

We can bring `toAppEvent` into play here:
```haskell
handleEvent . toAppEvent :: MonadState GameState m
                         => Event
                         -> m ()
```
which gives us
```haskell
traverse_ (handleEvent . toAppEvent) :: MonadState GameState m
                                     => [Event]
                                     -> m ()
```

That is what we need for the final version of `doEvents`:
```haskell
doEvents :: (MonadState GameState m, MonadIO m)
         => m ()
doEvents = do
  es <- liftIO pollEvents
  traverse_ (handleEvent . toAppEvent) es
```

## Updating the rendering

```haskell
background, render :: Renderer
                   -> GameState
                   -> IO ()
```

```haskell
background, render :: MonadIO m
                   => Renderer
                   -> GameState
                   -> m ()
```

```haskell
background, render :: (MonadState GameState m, MonadIO m)
                   => Renderer
                   -> m ()
```

```haskell
background, render :: (MonadReader Renderer m, MonadState GameState m, MonadIO m)
                   => m ()
```

We can do the transformations much like we did with `handleEvent`.

We start with what we have:
```haskell
background :: Renderer
           -> GameState
           -> IO ()
background r s = do
  rendererDrawColor r $= (toSDLColour . backgroundColour $ s)
  clear r

render :: Renderer
       -> GameState
       -> IO ()
render r s = do
  background r s
  present r
```
and then we use `liftIO` to move from `IO` to `MonadIO m`:
```haskell
background :: MonadIO m
           => Renderer
           -> GameState
           -> m ()
background r s = liftIO $ do
  rendererDrawColor r $= (toSDLColour . backgroundColour $ s)
  clear r

render :: MonadIO m
       => Renderer
       -> GameState
       -> m ()
render r s = liftIO $ do
  background r s
  present r
```

We then bring `MonadState GameState m` into the fold.

We can use `gets` for this:
```haskell
gets :: MonadState s m => (s -> a) -> m a 
```

Specialized to `GameState` we have:
```haskell
gets :: MonadState GameState m => (GameState -> a) -> m a 
```

Adding the `backgroundColour` accessor function in:
```haskell
backgroundColour :: GameState -> Colour
```
we get
```haskell
gets backgroundColur :: MonadState GameState m => m Colour
```
and we can even add `toSDLColour` in to get to the data we need:
```haskell
gets (toSDLColour . backgroundColur) :: MonadState GameState m => m (V4 Word8)
```

The version using `MonadState GameState m` is:
```haskell
background :: (MonadState GameState m, MonadIO m)
           => Renderer
           -> m ()
background r = do
  c <- gets $ toSDLColour . backgroundColour
  liftIO $ do
    rendererDrawColor r $= (toSDLColour . backgroundColour $ s)
    clear r
     
render :: (MonadState GameState m, MonadIO m
       => Renderer
       -> m ()
render r = liftIO $ do
  background r
  present r
```

We can save some plumbing of the `Renderer` in the surrounding code by introducing `MonadReader Renderer m`.

This will use `ask` to get the `Renderer` whenever we need it:
```haskell
ask :: MonadReader r m -> m r
```
which specializes to:
```haskell
ask :: MonadReader Renderer m -> m Renderer
```

The version using `MonadReader Renderer m` is:
```haskell
background :: (MonadReader Renderer m, MonadState GameState m, MonadIO m)
           => m ()
background = do
  r <- ask
  c <- gets $ toSDLColour . backgroundColour
  liftIO $ do
    rendererDrawColor r $= c
    clear r

render :: (MonadReader Renderer m, MonadState GameState m, MonadIO m)
       => m ()
render = do
  background
  r <- ask
  liftIO $
    present r
```

If we wound up drawing something completely independent of the `GameState`, we'd have:
```haskell
stateless :: (MonadReader Renderer m, MonadIO m)
          => m ()
```

In general, we don't add constraints that we aren't going to use.
This makes it easier to reason about the what a function (and the functions it may call) can or cannot do.

While we're reasoning about code, we know that `stateless` can't modify the `GameState`.
While we're refactoring code, we are prevented from accidentally modifying the `GameState` either directly or indirectly, because the compiler will stop us.

## Updating the main loop

The update to the main loop involves swapping the plumbing of the `Renderer` and `GameState` through everything and the addition of the various typeclass constraints that we've picked up along the way.

We also need to access the `hasQuit` field of the `GameState`.

We can use `gets` again for this:
```haskell
gets hasQuit :: MonadState GameState m => m Bool 
```

We go from this:
```haskell
gameLoop :: Renderer
         -> GameState
         -> IO ()
gameLoop r s = do
  -- update the GameState based on the events
  s' <- doEvents s
  -- render the game
  render r s'
  -- check to see if the user has quit
  let q = hasQuit s'
  -- if not, keep going
  unless q (gameLoop r s')
```
to this:
```haskell
gameLoop :: (MonadReader Renderer m, MonadState GameState m, MonadIO m)
         => m ()
gameLoop = do
  -- update the GameState based on the events
  doEvents
  -- render the game
  render
  -- check to see if the user has quit
  q <- gets hasQuit
  -- if not, keep going
  unless q gameLoop
```

We also add a function to set up and kick off the monad transformers:
```haskell
startGameLoop :: Renderer -> GameState -> IO ()
startGameLoop r s = evalStateT (runReaderT gameLoop r) s
```

## Updating `main`


If you've been browsing through the source code you'll notice that most of the code is in a library, and the `main` function is part of a module that uses that library to build an executable.

By supplying `startGameLoop` the `main` function doesn't need to know about anything `mtl` related.
The executable doesn't directly depend on the `mtl` package in that case.
That doesn't buy us much, but it's nice to know that `mtl` is just an implementation detail from the perspective of the executable.

Our old `main` function looks like this:
```haskell
main :: IO ()
main = do
  initialize [InitEverything]
  window <- createWindow "My SDL Application" defaultWindow
  r <- createRenderer window (-1) defaultRenderer
  gameLoop r defaultGameState
```
and the new version looks like this:
```haskell
main :: IO ()
main = do
  initialize [InitEverything]
  window <- createWindow "My SDL Application" defaultWindow
  r <- createRenderer window (-1) defaultRenderer
  startGameLoop r defaultGameState
```

# Conclusion

We've mostly swapped plumbing for typeclass constraints, although we're getting into a better position to make further changes.

[Next](./lens.html) we'll bring `lens` into the mix, and will also look at how mixing `mtl` and `lens` can provide some additional benefits.
