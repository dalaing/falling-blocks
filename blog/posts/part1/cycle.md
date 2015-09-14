---
title: Expanding on the example
published: 2015-09-14 12:00:00+10:00
---

# Introduction

[Previously](./refactor.html) we took the example from `sdl2` and refactored it somewhat.

In this post we'll add some additional functionality, in order to help motivate and explain some of the changes that will be made in the next few posts.

The change is straightforward: instead of having a fixed background colour, we'll now have a background colour that gets cycled whenever the user presses the space key.

The code is available [here](https://github.com/dalaing/falling-blocks/tree/master/code/part1/cycle).

# The changes

## Modelling our colours

We're going to add our own colour type, for reasons that will become apparent later.

For now, we'll just have three colours:
```haskell
data Colour =
    Red
  | Green
  | Blue
  deriving (Eq, Show)
```

We want to be able to cycle through them, so we define a function for that:
```haskell
nextColour :: Colour
           -> Colour
nextColour Red   = Green
nextColour Green = Blue
nextColour Blue  = Red
```

Eventually we'll need to be able to convert from our abstract representation of colour to the representation that SDL works with:
```haskell
toSDLColour :: Colour
            -> V4 Word8
toSDLColour Red   = V4 255 0 0 255
toSDLColour Green = V4 0 255 0 255
toSDLColour Blue  = V4 0 0 255 255
```

## Updating the game state

We need to add the background colour to our game state:
```haskell
data GameState = GameState {
    -- | The background colour
    backgroundColour :: Colour
    -- | Whether or not the user has quit
  , hasQuit          :: Bool
  } deriving (Eq, Show)
```

The default game state gets updated to reflect this:
```haskell
defaultGameState :: GameState
defaultGameState = GameState Blue False
```

## Updating the event handling

We'll add a new event that indicates that the background colour should be cycled:
```haskell
data AppEvent =
  -- | Cycle event
    AppCycle
  -- | Quit event
  | AppQuit
  -- | Any SDL event we don't handle
  | AppOther Event
  deriving (Eq, Show)
```

This event gets triggered when the user presses the space key:
```haskell
toAppEvent :: Event
           -> AppEvent
toAppEvent event =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      if keyboardEventKeyMotion keyboardEvent == Pressed
      then
        case keysymKeycode (keyboardEventKeysym keyboardEvent) of
          KeycodeSpace -> AppCycle
          KeycodeQ     -> AppQuit
          _            -> AppOther event
      else
        AppOther event
    _ -> AppOther event
```

We now need to update `handleEvent` to update the game state based on this event.

If we are processing the `AppCycle` event, take the old background colour, apply the `nextColour` function to it, and use the result as the new background colour:
```haskell
handleEvent :: AppEvent
            -> GameState
            -> GameState
handleEvent AppCycle gs = gs { backgroundColour = nextColour . backgroundColour $ gs }
handleEvent AppQuit  gs = gs { hasQuit = True }
handleEvent _        gs = gs
```

## Updating the rendering

Unsurprisingly, we now need to have access to the background colour while rendering the background.
We also have to convert it from our abstract `Colour` type to something SDL can deal with, using `toSDLColour`:
```haskell
background :: Renderer
           -> GameState
           -> IO ()
background r s = do
  rendererDrawColor r $= (toSDLColour . backgroundColour $ s)
  clear r
```

The `render` function needs to receive the `GameState` as an argument, so that it can pass the state along to any other rendering functions which need it:
```haskell
render :: Renderer
       -> GameState
       -> IO ()
render r s = do
  background r s
  present r
```

## Updating the main loop

The only other change we have to make is to pass the `GameState` to the `render` function in our game loop:
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

# Conclusion

That was a fairly small change in functionality, and we're in a much better place for motivating the changes that are coming up.

[Next](./mtl.html) we'll add monad transformers into the picture.
