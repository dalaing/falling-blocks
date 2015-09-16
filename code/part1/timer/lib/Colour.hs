-- | The 'Colour' module deals with the colours in our application.
module Colour (
    Colour(..)
  , nextColour
  , toSDLColour
  ) where

-- from 'base'
import           Data.Word (Word8)

-- from 'linear'
import           Linear    (V4 (..))

-- | The application specific colours.
data Colour =
    Red
  | Green
  | Blue
  deriving (Eq, Show)

-- | Finds the next 'Colour' in the cycle.
nextColour :: Colour -> Colour
nextColour Red   = Green
nextColour Green = Blue
nextColour Blue  = Red


-- | Converts a 'Colour' to an SDL colour.
toSDLColour :: Colour -> V4 Word8
toSDLColour Red   = V4 255 0 0 255
toSDLColour Green = V4 0 255 0 255
toSDLColour Blue  = V4 0 0 255 255

