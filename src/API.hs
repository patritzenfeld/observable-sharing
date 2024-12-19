
module API (Drawable(..)) where


import Data.Text (Text)
import Types (Color, Font, TextStyle)



class Drawable a where
  rectangle :: Double -> Double -> a
  solidRectangle :: Double -> Double -> a
  thickRectangle :: Double -> Double -> Double -> a
  circle :: Double -> a
  solidCircle :: Double -> a
  arc :: Double -> Double -> Double -> a
  sector :: Double -> Double -> Double -> a
  thickArc :: Double -> Double -> Double -> Double -> a
  lettering :: Text -> a
  styledLettering :: TextStyle -> Font -> Text -> a
  thickCircle :: Double -> Double -> a
  translated :: Double -> Double -> a -> a
  colored :: Color -> a -> a
  coloured :: Color -> a -> a
  coloured = colored
  dilated :: Double -> a -> a
  scaled :: Double -> Double -> a -> a
  rotated :: Double -> a -> a
  pictures :: [a] -> a
  (&) :: a -> a -> a
  coordinatePlane :: a
  blank :: a
