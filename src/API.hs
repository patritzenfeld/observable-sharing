
module API (Drawable(..)) where


import Data.Text (Text)
import Types (Color, Font, Point, TextStyle)



class Drawable a where
  rectangle :: Double -> Double -> a
  solidRectangle :: Double -> Double -> a
  thickRectangle :: Double -> Double -> Double -> a
  circle :: Double -> a
  solidCircle :: Double -> a
  arc :: Double -> Double -> Double -> a
  sector :: Double -> Double -> Double -> a
  thickArc :: Double -> Double -> Double -> Double -> a
  curve :: [Point] -> a
  thickCurve :: Double -> [Point] -> a
  closedCurve :: [Point] -> a
  thickClosedCurve :: Double -> [Point] -> a
  solidClosedCurve :: [Point] -> a
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
