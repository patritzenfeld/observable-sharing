
module API (Drawable(..)) where


import Data.Text (Text)
import Types (Color)



class Drawable a where
  rectangle :: Double -> Double -> a
  solidRectangle :: Double -> Double -> a
  thickRectangle :: Double -> Double -> Double -> a
  circle :: Double -> a
  solidCircle :: Double -> a
  lettering :: Text -> a
  thickCircle :: Double -> Double -> a
  translated :: Double -> Double -> a -> a
  colored :: Color -> a -> a
  dilated :: Double -> a -> a
  scaled :: Double -> Double -> a -> a
  rotated :: Double -> a -> a
  pictures :: [a] -> a
  (&) :: a -> a -> a
  coordinatePlane :: a
  blank :: a
