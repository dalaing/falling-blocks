---
title: Adding `lens`
published: 2015-09-14 12:00:00+10:00
---

# Introduction

[Previously](./mtl.html) we started using `mtl` as part of our running example.

In this post we'll start using `lens`.
We'll be making fairly basic usage of `lens` for now, although we'll also cover a nice trick combining `mtl` and `lens` that we'll be using later on.

The code is available [here](https://github.com/dalaing/falling-blocks/tree/master/code/part1/lens).

## Basic lensing

Lenses are pretty handy.
One of the ways we can view a lens is as a first-class combination of a getter and a setter that we can compose.

### Updating the game state 

First we need some lenses.
We'll add them to our `GameState`, by changing from this:
```haskell
data GameState = GameState {
    -- | The background colour
    backgroundColour :: Colour
    -- | Whether or not the user has quit
  , hasQuit          :: Bool
  } deriving (Eq, Show)
```
to this:
```haskell
data GameState = GameState {
    -- | The background colour
    _backgroundColour :: Colour
    -- | Whether or not the user has quit
  , _hasQuit          :: Bool
  } deriving (Eq, Show)

makeLenses ''GameState
```

We add the underscores so that the Template Haskell used inside `makeLenses` can work out which fields we want lenses for.

The result in this case will be two lenses:
```haskell
backgroundColour :: Lens' GameState Colour
```
and
```haskell
hasQuit :: Lens' GameState Bool
```

Lets put them to work.

### Updating the event handling

The first thing we will do is update `handleEvent`:
```haskell
handleEvent :: MonadState GameState m
            => AppEvent
            -> m ()
handleEvent AppCycle = modify $ \gs -> gs { backgroundColour = nextColour . backgroundColour $ gs }
handleEvent AppQuit  = modify $ \gs -> gs { hasQuit = True }
handleEvent _        = return ()
```

We can use `.~` to set a value at the target of a lens, so we can replace
```haskell
gs { hasQuit = True }
```
with
```haskell
gs & hasQuit .~ True
```

We can also use `%~` to apply a function to the target of a lens, so we also replace
```haskell
gs { backgroundColour = nextColour . backgroundColour $ gs }
```
with
```haskell
gs & backgroundColour %~ nextColour
```

This leaves us with:
```haskell
handleEvent :: MonadState GameState m
            => AppEvent
            -> m ()
handleEvent AppCycle = modify $ \gs -> gs & backgroundColour %~ nextColour
handleEvent AppQuit  = modify $ \gs -> gs & hasQuit .~ True
handleEvent _        = return ()
```

There are also variants that work with the value carried around by the `MonadState`, which removes the need to call `modify`.

The convention is to change from `~` to `=` in the operators, so we have `.=` to set the value at the target of a lens and `%=` to apply a function to the value at the target of a lens.

This means we can replace
```haskell
modify $ \gs -> gs & hasQuit .~ True
```
with
```haskell
hasQuit .= True
```
and
```haskell
modify $ \gs -> gs & backgroundColour %~ nextColour
```
with
```haskell
backgroundColour %= nextColour
```

Our final version is:
```haskell
handleEvent :: MonadState GameState m
            => AppEvent
            -> m ()
handleEvent AppCycle = backgroundColour %= nextColour
handleEvent AppQuit  = hasQuit .= True
handleEvent _        = return ()
```

### Updating the rendering

```haskell
background :: (MonadReader Renderer m, MonadState GameState m, MonadIO m)
           => m ()
background = do
  r <- ask
  c <- gets $ toSDLColour . backgroundColour
  liftIO $ do
    rendererDrawColor r $= c
    clear r
```

```haskell
background :: (MonadReader Renderer m, MonadState GameState m, MonadIO m)
           => m ()
background = do
  r <- ask
  c <- gets $ toSDLColour . (^. backgroundColour)
  liftIO $ do
    rendererDrawColor r $= c
    clear r
```

```haskell
background :: (MonadReader Renderer m, MonadState GameState m, MonadIO m)
           => m ()
background = do
  r <- ask
  c <- gets $ \gs -> gs ^. backgroundColour . to toSDLColour
  liftIO $ do
    rendererDrawColor r $= c
    clear r
```

```haskell
sdlBackgroundColour :: Lens' GameState (V4 Word8)
sdlBackgroundColour = backgroundColour . to toSDLColour

background :: (MonadReader Renderer m, MonadState GameState m, MonadIO m)
           => m ()
background = do
  r <- ask
  c <- gets $ \gs -> gs ^. sdlBackgroundColour
  liftIO $ do
    rendererDrawColor r $= c
    clear r
```

```haskell
sdlBackgroundColour :: Lens' GameState (V4 Word8)
sdlBackgroundColour = backgroundColour . to toSDLColour

background :: (MonadReader Renderer m, MonadState GameState m, MonadIO m)
           => m ()
background = do
  r <- ask
  c <- use sdlBackgroundColour
  liftIO $ do
    rendererDrawColor r $= c
    clear r
```

### Updating the main loop

The change to the game loop is just a matter of swapping from `gets hasQuit`:
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
to `use hasQuit`:
```haskell
gameLoop :: (MonadReader Renderer m, MonadState GameState m, MonadIO m)
         => m ()
gameLoop = do
  -- update the GameState based on the events
  doEvents
  -- render the game
  render
  -- check to see if the user has quit
  q <- use hasQuit
  -- if not, keep going
  unless q gameLoop
```
and we're done.

## Lensing with class

### Preparing for more configuration

We're about to get some more data that we'll be passing around with our `MonadReader`, so we're going to create a `Config` type to carry that around:
```haskell
data Config = Config {
    _renderer :: Renderer
  } deriving (Eq, Show)

makeClassy ''Config
```

The use of `makeClassy` is new here.

It sets up:
```haskell
class HasConfig a where
  config :: Lens' a Config
  renderer :: Lens' a Renderer

instance HasConfig Config where
  config = id
```

We also add a helper to create `Config` values:
```haskell
mkConfig :: Renderer -> Config
mkConfig = Config
```

### Changing the rendering

Most of the effects of this change occur to the rendering functions.

We have to change the constraint from `MonadReader Renderer m` to the pair of constraints `(HasConfig c, MonadReader c m)`.
We also have to change the use of `ask` - which gets hold of the `Renderer` to `view renderer`.

The `view` function allows us to read from the target of a lens, and it works with `MonadReader` out of the box.

The change for the `background` function is from:
```haskell
background :: (MonadReader Renderer m, MonadState GameState m, MonadIO m)
           => m ()
background = do
  r <- ask
  c <- use $ backgroundColour . to toSDLColour
  liftIO $ do
    rendererDrawColor r $= c
    clear r
```
to:
```haskell
background :: (HasConfig c, MonadReader c m, MonadState GameState m, MonadIO m)
           => m ()
background = do
  r <- view renderer
  c <- use $ backgroundColour . to toSDLColour
  liftIO $ do
    rendererDrawColor r $= c
    clear r
```

We also make the same change to `render`.

### Changing the main loop

The main loop gets a similar change to the contraint, where:
```haskell
gameLoop :: (MonadReader Renderer m, MonadState GameState m, MonadIO m)
         => m ()
```
becomes:
```haskell
gameLoop :: (HasConfig c, MonadReader c m, MonadState GameState m, MonadIO m)
         => m ()
```

We also update our helper from:
```haskell
startGameLoop :: Renderer -> GameState -> IO ()
startGameLoop r = evalStateT (runReaderT gameLoop r)
```
to:
```haskell
startGameLoop :: Config -> GameState -> IO ()
startGameLoop c = evalStateT (runReaderT gameLoop c)
```

This results in a flow on change to `main`, from:
```haskell
main :: IO ()
main = do
  initialize [InitEverything]
  window <- createWindow "My SDL Application" defaultWindow
  r <- createRenderer window (-1) defaultRenderer
  startGameLoop r defaultGameState
```
to:
```haskell
main :: IO ()
main = do
  initialize [InitEverything]
  window <- createWindow "My SDL Application" defaultWindow
  r <- createRenderer window (-1) defaultRenderer
  startGameLoop (mkConfig r) defaultGameState
```

### Where classy wins

The wins really turn up when we start nesting data more.

Imagine if we expanded the rendering information in the `Config`, creating a `RendererConfig` type with classy lenses set up.

We could then create:
```haskell
instance HasRendererConfig Config where
  rendererConfig = renderer
```

Now we can work with `MonadReader Config m`, but functions that only need access to the `RendererConfig` can use `(HasRendererConfig r, MonadReader r m)` as the context and still work with `MonadReader Config m`.

This is another case where we can make things easier to reason about and easier to refactor by limiting the amount of context that a function gets to the minimum that it needs.

# Conclusion

Now we've got lenses in our code.
It looks like small wins so far, but they'll add up as things get more complex.

<!--[Next](./timer.html) we'll drive the updates using a timer, and will introduce STM along the way.-->


