{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}

module Normalize (
  module Normalize
  ) where


import Data.Containers.ListUtils        (nubOrd)
import Data.List                        (sort)
import Data.Text                        (Text)

import API                              (Drawable(..))
import Types                            (Color(..), Point)


newtype Size = Size Double deriving (Ord)


data Thickness
  = Normal
  | Thick
  deriving (Show,Eq,Ord)


data ShapeKind
  = Hollow Thickness
  | Solid
  deriving (Eq,Ord,Show)


data Angle
  = ToQuarter Double
  | ToHalf Double
  | ToThreeQuarter Double
  | ToFull Double
  deriving (Ord)


data Moved
  = Neg Double
  | Pos Double
  | Zero
  deriving (Ord)


data DirectionV = South deriving (Eq,Ord,Show)
data DirectionH = West | East deriving (Eq,Ord,Show)

data Direction = Direction {
  vertical :: Maybe DirectionV,
  horizontal :: Maybe DirectionH
} deriving (Eq,Ord)


data RelativePicSpec
  = Is RelativePicSpec Direction RelativePicSpec
  | PicSpec NormalizedPicture
  deriving(Eq,Ord)

type AbsPoint = (Moved,Moved)


instance Show Direction where
  show Direction{..} = case (vertical, horizontal) of
    (Nothing, Nothing) -> "OnTop"
    (Nothing, Just a)  -> show a
    (Just a, Nothing)  -> show a
    (Just a, Just b)   -> show a ++ show b


instance Show RelativePicSpec where
  show (PicSpec p) = show p
  show (Is p1 dir p2) = show p1 ++ " is " ++ show dir ++ " of " ++ show p2


(===) :: NormalizedPicture -> NormalizedPicture -> Bool
p1 === p2 = toRelative p1 == toRelative p2


instance Show Size where
  show _ = "Size"


instance Eq Size where
  _ == _ = True


instance Show Moved where
  show Zero    = "Zero"
  show (Neg _) = "Neg"
  show (Pos _) = "Pos"

instance Eq Moved where
  (Neg _) == (Neg _) = True
  (Pos _) == (Pos _) = True
  _       == _       = False


instance Num Moved where
  Zero + a = a
  a + Zero = a
  Pos a + Pos b = Pos $ a+b
  Neg a + Neg b = Neg $ a+b
  Pos a + Neg b
    | a==b = Zero
    | a < b = Neg $ b-a
    | otherwise = Pos $ a-b
  a@(Neg _) + b@(Pos _) = b + a

  abs Zero = Zero
  abs (Neg a) = Pos a
  abs a = a

  Zero * _ = Zero
  _ * Zero = Zero
  Pos a * Pos b = Pos $ a*b
  Neg a * Neg b = Pos $ a*b
  Pos a * Neg b = Neg $ a*b
  a * b = b*a

  signum Zero = Zero
  signum (Pos _) = Pos 1
  signum (Neg _) = Neg 1

  negate Zero = Zero
  negate (Pos a) = Neg a
  negate (Neg a) = Pos a

  fromInteger i
    | i == 0 = Zero
    | i < 0 = Neg $ fromIntegral i
    | otherwise = Pos $ fromIntegral i


halve :: Moved -> Moved
halve Zero = Zero
halve (Pos a) = Pos $ a/2
halve (Neg a) = Neg $ a/2


instance Eq Angle where
  (ToQuarter _)      == (ToQuarter _)      = True
  (ToHalf _)         == (ToHalf _)         = True
  (ToThreeQuarter _) == (ToThreeQuarter _) = True
  (ToFull _)         == (ToFull _)         = True
  _                  == _                  = False


instance Show Angle where
  show (ToQuarter _) = "NoneToQuarter"
  show (ToHalf _) = "QuarterToHalf"
  show (ToThreeQuarter _) = "HalfToThreeQuarter"
  show (ToFull _) = "ThreeQuarterToFull"


data NormalizedPicture
  = Rectangle !ShapeKind !Size !Size
  | Circle !ShapeKind !Size
  | Lettering !Text
  | Color !Color !NormalizedPicture
  | Translate !Moved !Moved !NormalizedPicture
  | Scale !Double !Double !NormalizedPicture
  | Rotate !Angle !NormalizedPicture
  | Pictures [NormalizedPicture]
  | CoordinatePlane
  | Blank
  | Polygon !ShapeKind [AbsPoint]
  | ClosedCurve !ShapeKind [AbsPoint]
  | Polyline [AbsPoint] !Thickness
  | Curve !ShapeKind [AbsPoint]
  | Arc !ShapeKind !Angle !Angle !Size
  | Reflect !Angle !NormalizedPicture
  | Clip !Size !Size !NormalizedPicture
  deriving (Show,Eq,Ord)


thickness :: (Eq a, Fractional a) => a -> Thickness
thickness d
  | d /= 0 = Thick
  | otherwise = Normal


instance Drawable NormalizedPicture where

  pictures [] = blank
  pictures [x] = x
  pictures xs = Pictures xs

  Blank & p = p
  p & Blank = p
  p1 & p2 = Pictures $ ps1 ++ ps2
    where
      ps1 = case p1 of
        Pictures ps -> ps
        _           -> [p1]
      ps2 = case p2 of
        Pictures ps -> ps
        _           -> [p2]

  blank = Blank

  coordinatePlane = CoordinatePlane

  circle 0 = blank
  circle r = Circle (Hollow Normal) $ toSize r

  solidCircle 0 = blank
  solidCircle r = Circle Solid $ toSize r

  thickCircle 0 _ = blank
  thickCircle t r = Circle (Hollow $ thickness t) $ toSize r

  rectangle 0 _ = blank
  rectangle _ 0 = blank
  rectangle l w  = Rectangle (Hollow Normal) (toSize l) $ toSize w

  solidRectangle 0 _ = blank
  solidRectangle _ 0 = blank
  solidRectangle l w = Rectangle Solid (toSize l) $ toSize w

  thickRectangle _ 0 _ = blank
  thickRectangle _ _ 0 = blank
  thickRectangle t l w = Rectangle (Hollow $ thickness t) (toSize l) $ toSize w

  arc _ _ 0 = blank
  arc a1 a2 r
    | a1 == a2  = blank
    | a1 > a2 = arc a2 a1 r
    | abs (a1 - a2) >= 2*pi = circle r
    | otherwise = Arc (Hollow Normal) (toAngle a1) (toAngle a2) (toSize r)

  sector _ _ 0 = blank
  sector a1 a2 r
    | a1 == a2  = blank
    | a1 > a2 = sector a2 a1 r
    | abs (a1 - a2) >= 2*pi = solidCircle r
    | otherwise = Arc Solid (toAngle a1) (toAngle a2) (toSize r)

  thickArc _ _ _ 0 = blank
  thickArc t a1 a2 r
    | a1 == a2  = blank
    | a1 > a2 = thickArc t a2 a1 r
    | abs (a1 - a2) >= 2*pi = thickCircle t r
    | otherwise = Arc (Hollow $ thickness t) (toAngle a1) (toAngle a2) (toSize r)

  curve = handlePointList $ Curve $ Hollow Normal

  thickCurve t = handlePointList $ Curve $ Hollow $ thickness t

  closedCurve = handlePointList $ ClosedCurve $ Hollow Normal

  solidClosedCurve = handlePointList $ ClosedCurve Solid

  thickClosedCurve t = handlePointList $ ClosedCurve $ Hollow $ thickness t

  lettering "" = blank
  lettering t  = Lettering t

  styledLettering _ _ "" = blank
  styledLettering _ _ t = Lettering t

  translated 0 0 p = p
  translated x y p = case p of
    Translate a b q -> translated (x + getExactPos a) (y + getExactPos b) q
    Pictures ps     -> Pictures $ map (translated x y) ps
    Blank           -> Blank
    a               -> Translate (toPosition x) (toPosition y) a

  colored c p = case p of
    Translate x y q -> Translate x y $ colored c q
    Rotate a q     -> Rotate a $ colored c q
    Color _ q      -> colored c q
    Pictures ps    -> Pictures $ map (colored c) ps
    Blank          -> Blank
    q              -> Color c q

  dilated fac = scaled fac fac

  scaled 0 _ _ = blank
  scaled _ 0 _ = blank
  scaled 1 1 p = p
  scaled fac1 fac2 p = case p of
    Scale f1 f2 q    -> scaled (f1*abs fac1) (f2* abs fac2) q
    Translate x y q  -> Translate
                         (toPosition $ getExactPos x*fac1)
                         (toPosition $ getExactPos y*fac2)
                         $ scaled fac1 fac2 q
    Blank            -> Blank
    Pictures ps      -> Pictures $ map (scaled fac1 fac2) ps
    a                -> Scale (abs fac1) (abs fac2) a

  rotated 0 p = p
  rotated a p = case p of
    Rotate a2 q     -> rotated (a + getExactAngle a2) q
    Translate x y q -> Translate
                        (toPosition $ getExactPos x*cos a - getExactPos y*sin a)
                        (toPosition $ getExactPos x*sin a + getExactPos y*cos a)
                        $ rotated a q
    Pictures ps     -> Pictures $ map (rotated a) ps
    q               -> Rotate (toAngle a) q




handlePointList :: Drawable a => ([AbsPoint] -> a) -> [Point] -> a
handlePointList f ps
    | length reduced < 2 = blank
    | otherwise = f $ map toAbstractPoint reduced
  where
    reduced = sort $ nubOrd ps


modTwoPi :: Double -> Double
modTwoPi d
  | d == 0 || d == 2*pi = 0
  | d < 2*pi = d
  | otherwise = modTwoPi (d-2*pi)


toAngle :: Double -> Angle
toAngle a
  | a < 0 = toAngle (a+2*pi)
  | a <= pi/2 = ToQuarter a
  | a <= pi = ToHalf a
  | a <= 3*pi/2 = ToThreeQuarter a
  | a < 2*pi = ToFull a
  | otherwise = toAngle (a-2*pi)


addToAngle :: Double -> Angle -> Angle
addToAngle d a = toAngle $ d + getExactAngle a


getExactAngle :: Angle -> Double
getExactAngle (ToQuarter a) = a
getExactAngle (ToHalf a) = a
getExactAngle (ToThreeQuarter a) = a
getExactAngle (ToFull a) = a


getExactPos :: Moved -> Double
getExactPos Zero    = 0
getExactPos (Neg d) = -d
getExactPos (Pos d) =  d


toPosition :: Double -> Moved
toPosition d
  | d == 0 = Zero
  | d < 0 = Neg $ abs d
  | otherwise = Pos d


toAbstractPoint :: Point -> AbsPoint
toAbstractPoint (x,y) = (toPosition x, toPosition y)

middle :: Moved -> Moved -> Moved
middle Zero a = halve a
middle a Zero = halve a
middle a b = let dist = abs $ a-b
  in a - (signum a *halve dist)


internallyEqual :: Moved -> Moved -> Bool
internallyEqual (Pos a) (Pos b) = a==b
internallyEqual (Neg a) (Neg b) = a==b
internallyEqual Zero Zero = True
internallyEqual _ _ = False


southOf :: RelativePicSpec -> RelativePicSpec -> RelativePicSpec
southOf p1 = Is p1 (Direction (Just South) Nothing)


northOf :: RelativePicSpec -> RelativePicSpec -> RelativePicSpec
northOf = flip southOf


westOf :: RelativePicSpec -> RelativePicSpec -> RelativePicSpec
westOf p1 = Is p1 (Direction  Nothing (Just West))


eastOf :: RelativePicSpec -> RelativePicSpec -> RelativePicSpec
eastOf = flip westOf


onTopOf :: RelativePicSpec -> RelativePicSpec -> RelativePicSpec
onTopOf p1 = Is p1 (Direction Nothing Nothing)


southwestOf :: RelativePicSpec -> RelativePicSpec -> RelativePicSpec
southwestOf p1 = Is p1 (Direction (Just South) (Just West))


southeastOf :: RelativePicSpec -> RelativePicSpec -> RelativePicSpec
southeastOf p1 = Is p1 (Direction (Just South) (Just East))


northwestOf :: RelativePicSpec -> RelativePicSpec -> RelativePicSpec
northwestOf = flip southeastOf


northeastOf :: RelativePicSpec -> RelativePicSpec -> RelativePicSpec
northeastOf = flip southwestOf


toSize :: Double -> Size
toSize = Size


yellow :: Color
yellow = Yellow


green :: Color
green = Green


red :: Color
red = Red


toRelative :: NormalizedPicture -> [RelativePicSpec]
toRelative p = case p of
  Pictures ps -> sort $ relativePosition ps
  a           -> [PicSpec a]


stripTranslation :: NormalizedPicture -> NormalizedPicture
stripTranslation (Translate _ _ p) = p
stripTranslation p                 = p


relativePosition :: [NormalizedPicture] -> [RelativePicSpec]
relativePosition [] = []
relativePosition (Translate x y p:ps) = othersTrans ++ relativePosition ps
  where
    asCenter (Translate a b pic) =
      translated (getExactPos $ a-x) (getExactPos $ b-y) pic
    asCenter pic = translated (getExactPos $ -x) (getExactPos $ -y) pic
    othersTrans = map (\pic ->
        orientation (asCenter pic) (PicSpec p) $ PicSpec $ stripTranslation pic
        )
      ps
relativePosition (p:ps) = others ++ relativePosition ps
  where
    others = map (\x -> orientation x (PicSpec p) $ PicSpec $ stripTranslation x) ps


orientation
  :: NormalizedPicture
  -> ( RelativePicSpec
    -> RelativePicSpec
    -> RelativePicSpec
     )
orientation (Translate a b _) = case (a,b) of
      (Zero, Neg _)   -> northOf
      (Zero, Pos _)   -> southOf
      (Neg _, Zero)   -> eastOf
      (Pos _, Zero)   -> westOf
      (Pos _,  Pos _) -> southwestOf
      (Pos _, Neg _)  -> northwestOf
      (Neg _, Pos _)  -> southeastOf
      (Neg _, Neg _)  -> northeastOf
      _               -> error $
                           "This should never happen. " ++
                           "translated smart constructor wasn't used."
orientation _ = onTopOf


sampleSolution :: Drawable a => a
sampleSolution = translated 0 6 (colored yellow (solidCircle 1))
               & colored green (solidRectangle 20 2)


example1 :: Drawable a => a
example1 = colored yellow (solidCircle 2)
         & translated 0 (-5) (colored green (solidRectangle 12 3))


example2 :: Drawable a => a
example2 = translated 0 6 (colored yellow(solidCircle 1))
         & colored green (solidRectangle 12 2)


example3 :: Drawable a => a
example3 = grass & sun
  where
    grass = colored green (solidRectangle 20 2)
    sun = translated 0 7 (colored yellow (solidCircle 1.5))


example4 :: Drawable a => a
example4 = ebene & translated 0 9 sonne
  where
    boden = solidRectangle 20 2
    ebene = colored green boden
    sonne = colored yellow (solidCircle 1)


example5 :: Drawable a => a
example5 = colored yellow (solidCircle 1)
         & translated 0 (-3) (colored green (solidRectangle 8 1.5))


threeCircles :: Drawable a => a
threeCircles = colored red (solidCircle 1)
             & translated 2 4 (colored green $ solidCircle 1)
             & translated 0 3 (colored yellow $ solidCircle 1)
