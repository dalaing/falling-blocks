---
title: Breaking up the initial example
published: 2015-09-14 12:00:00+10:00
---
# The `sdl2` example

The `sdl2` packages comes with the following example

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL
import Linear
import Control.Monad (unless)

main :: IO ()
main = do
  initialize [InitEverything]
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer

appLoop :: Renderer -> IO ()
appLoop renderer = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = not (null (filter eventIsQPress events))
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  present renderer
  unless qPressed (appLoop renderer)
```

# Refactoring

We can refactor this to prepare a little for what we're about to do later on.

This will happen in a number of stages, and will look like _massive_ over-engineering.

I'm heading down that path because:

- it will make future changes easier
- I've already put in the time to learn these techniques, so the cost for using these techniques is quite reasonable for me

I'm hoping that this series helps motivate some people to dig further into learning the various initially-daunting-looking tools.
Not everyone will want to spend the time or think that the value of the tools is worth the time, but that's fine.

Everyone else can strap in and come for a ride.

For now we're going to separate out

- the game
state
- the event handling
- the rendering

so that we have something pretty close to a model-view-controller design.

The code is available [here](https://github.com/dalaing/falling-blocks/tree/master/code/part1/refactor).

## The game state

The game state is pretty simple at this point.
We just need to track whether or not we have quit.

```haskell
data GameState = GameState {
    hasQuit :: Bool 
  } deriving (Eq, Show)
```

We also add a default `GameState`:
```haskell
defaultGameState :: GameState
defaultGameState = GameState False
```

## Our own event type

We set up our own even type so that we can be explicit about the information that is important to us:
```haskell
data AppEvent =
    AppQuit
  | AppOther Event
  deriving (Eq, Show)
```

We have `AppOther` in there as a catch-all.

It's easy to convert from SDL events to our own event:
```haskell
toAppEvent :: Event
           -> AppEvent
toAppEvent event =
  case eventPayload event of
      KeyboardEvent keyboardEvent ->
        if keyboardEventKeyMotion keyboardEvent == Pressed
        then
          case keysymKeycode (keyboardEventKeysym keyboardEvent) of
              KeycodeQ -> AppQuit
              _ -> AppOther event
        else
          AppOther event
      _ -> AppOther event
```

One of the two main things we need to deal with is the connection between our application events and the game state.
The other things is doing the rendering based on our game state, but we don't have to worry about it that in this case.

We connect the application events to the game state with `handleEvent`:
```haskell
handleEvent :: AppEvent
            -> GameState
            -> GameState
handleEvent AppQuit gs = gs { hasQuit = true }
handleEvent _       gs = gs
```

We have 

- `pollEvents` from `sdl2` to gather the pending SDL events
- `toAppEvent` to convert SDL events to our application events
- `handleEvent` to apply our application events to our game state

We can tie them all together to get something that will update the `GameState`:
```haskell
doEvents :: GameState
         -> IO GameState
doEvents gs = do
  es <- pollEvents
  return $ foldr (handleEvent . toAppEvent) gs es
```

## Rendering the game state

We create a main function to do our rendering, which will call all the other rendering functions and then call `present` to update the screen.
```haskell
render :: Renderer
     -> IO ()
render r = do
  background r
  present r
```

For now all we have to display is the background:
```haskell
background :: Renderer
           -> IO ()
background r = do
  rendererDrawColor r $= V4 0 0 255 255
  clear r
```
but later on we'll have drawing functions to render the relevant information from our game state.

## Tying it all together

Since `doEvents` ties the events to the game state and `draw` renders the game state, we need to call both of these in our main loop, and continue until the user quits:

```haskell
gameLoop :: Renderer
         -> GameState
         -> IO ()
gameLoop r s = do
  -- render the game
  render r
  -- update the GameState based on the events
  s' <- doEvents s
  -- check to see if the user has quit
  let q = hasQuit s'
  -- if not, keep going
  unless q (gameLoop r s')
```

The last thing to do is to modify the `main` function so that our initial state gets passed to the game loop:
```haskell
main :: IO ()
main = do
  initialize [InitEverything]
  window <- createWindow "My SDL Application" defaultWindow
  r <- createRenderer window (-1) defaultRenderer
  gameLoop r defaultGameState
```

And we're done.

# Conclusion

We've exploded the code, the event handling code and the rendering code are separated by the code that deals with the game state.

We still have lots more to do, but it's a good first step.

[Next](./cycle.html) we'll add a little more functionality, in order to be able to better demonstrate some of the tools and techniques we'll be using.
