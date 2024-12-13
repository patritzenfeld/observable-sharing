{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}

module Picture (module Picture) where


import Data.Text (Text)
import Data.List (sort)


data Thickness = Normal | Thick deriving (Show,Eq,Ord)
data Angle = ToQuarter Double | ToHalf Double | ToThreeQuarter Double | ToFull Double deriving (Ord)
data Moved = Neg Double | Pos Double | Zero deriving (Ord)
data Color = Yellow | Green | Red deriving (Eq,Show,Ord)
newtype Size = Size Double deriving(Ord)

data PlaneOrder = NormalizedPicture :> NormalizedPicture | Any deriving (Show,Eq)
data DirectionV = South deriving (Eq,Ord,Show)
data DirectionH = West | East deriving (Eq,Ord,Show)
data Direction = Direction {vertical :: Maybe DirectionV, horizontal :: Maybe DirectionH} deriving (Eq,Ord)
data RelativePicSpec = Is RelativePicSpec Direction RelativePicSpec | PicSpec NormalizedPicture deriving(Eq,Ord)


instance Show Direction where
  show Direction{..} = case (vertical, horizontal) of
    (Nothing, Nothing) -> "Ontop"
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



data NormalizedPicture
  = Rectangle !Thickness !Size !Size
  | SolidRectangle !Size !Size
  | Circle !Thickness !Size
  | SolidCircle !Size
  | Lettering !Text
  | Color !Color !NormalizedPicture
  | Translate !Moved !Moved !NormalizedPicture
  | Scale !Double !Double !NormalizedPicture
  | Rotate !Angle !NormalizedPicture
  | Pictures [NormalizedPicture]
  | CoordinatePlane
  | Blank
  {-
  | SolidPolygon [Point]
  | Polygon [Point] !Thickness
  | SolidClosedCurve [Point]
  | ClosedCurve [Point] !Thickness
  | Polyline [Point] !Thickness
  | Curve [Point] !Thickness
  | Sector !Angle !Angle !Size
  | Arc !Thickness !Angle !Angle !Size
  | Reflect !Angle !Picture
  | Clip !Double !Double !Picture
  | Sketch !Text !Text !Double !Double
-}
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
  circle r = Circle Normal $ toSize r

  solidCircle 0 = blank
  solidCircle r = SolidCircle $ toSize r

  thickCircle 0 _ = blank
  thickCircle t r = Circle (thickness t) $ toSize r

  rectangle 0 _ = blank
  rectangle _ 0 = blank
  rectangle l w  = Rectangle Normal (toSize l) $ toSize w

  solidRectangle 0 _ = blank
  solidRectangle _ 0 = blank
  solidRectangle l w = SolidRectangle (toSize l) $ toSize w

  thickRectangle _ 0 _ = blank
  thickRectangle _ _ 0 = blank
  thickRectangle t l w = Rectangle (thickness t) (toSize l) $ toSize w

  lettering "" = blank
  lettering t  = Lettering t

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
    Translate x y q  -> Translate (toPosition $ getExactPos x*fac1) (toPosition $ getExactPos y*fac2) $ scaled fac1 fac2 q
    Blank            -> Blank
    Pictures ps      -> Pictures $ map (scaled fac1 fac2) ps
    a                -> Scale (abs fac1) (abs fac2) a

  rotated 0 p = p
  rotated a p = case p of
    Rotate a2 q     -> rotated (a + getExactAngle a2) q
    Translate x y q -> Translate (toPosition $ getExactPos x*cos a - getExactPos y*sin a) (toPosition $ getExactPos x*sin a + getExactPos y*cos a) $ rotated a q
    Pictures ps     -> Pictures $ map (rotated a) ps
    q               -> Rotate (toAngle a) q


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



toSpec :: NormalizedPicture -> RelativePicSpec
toSpec = PicSpec


southOf :: RelativePicSpec -> RelativePicSpec -> RelativePicSpec
southOf p1 = Is p1 (Direction (Just South) Nothing)

northOf :: RelativePicSpec -> RelativePicSpec -> RelativePicSpec
northOf = flip southOf

westOf :: RelativePicSpec -> RelativePicSpec -> RelativePicSpec
westOf p1 = Is p1 (Direction  Nothing (Just West))

eastOf :: RelativePicSpec -> RelativePicSpec -> RelativePicSpec
eastOf = flip westOf

ontopOf :: RelativePicSpec -> RelativePicSpec -> RelativePicSpec
ontopOf p1 = Is p1 (Direction Nothing Nothing)

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
  a           -> [toSpec a]



stripTranslation :: NormalizedPicture -> NormalizedPicture
stripTranslation (Translate _ _ p) = p
stripTranslation p                 = p


relativePosition :: [NormalizedPicture] -> [RelativePicSpec]
relativePosition [] = []
relativePosition (Translate x y p:ps) = othersTrans ++ relativePosition ps
  where
    asCenter (Translate a b pic) = translated (getExactPos $ a-x) (getExactPos $ b-y) pic
    asCenter pic = translated (getExactPos $ -x) (getExactPos $ -y) pic
    othersTrans = map (\pic -> orientation (asCenter pic) (toSpec p) $ toSpec $ stripTranslation pic) ps
relativePosition (p:ps) = others ++ relativePosition ps
  where
    others = map (\x -> orientation x (toSpec p) $ toSpec $ stripTranslation x) ps


{-
relativeAlignment :: [Picture] -> RelativePicSpec
relativeAlignment [] = toSpec blank
relativeAlignment [p] = case p of
  Translate _ _ q -> toSpec q
  _               -> toSpec p
relativeAlignment (p1:p2:ps) = case (p1,p2) of
  (Translate x1 y1 q, Translate x2 y2 q2) ->
    let
      newX = middle x1 x2
      newY = middle y1 y2
      newP2 = Translate newX newY q2
    in
      orientation (samePos x1 x2) (samePos y1 y2) (toSpec q) (relativeAlignment $ newP2:ps)
  (Translate x y q,_) ->
    let
      newX = middle x Zero
      newY = middle y Zero
      newP2 = Translate newX newY p2
    in
      orientation x y (toSpec q) (relativeAlignment (newP2:ps))
  (_, Translate {}) -> relativeAlignment (p2:p1:ps)
  (_,_) -> relativeAlignment (p2:ps) `ontopOf` toSpec p1
  where
    samePos a b
      |internallyEqual a b = Zero
      | otherwise = abs (a-b)
-}


orientation
  :: NormalizedPicture
  -> ( RelativePicSpec
    -> RelativePicSpec
    -> RelativePicSpec
     )
orientation (Translate a b _) = case (a,b) of
      (Zero, Neg _) -> northOf
      (Zero, Pos _) -> southOf
      (Neg _, Zero) -> eastOf
      (Pos _, Zero) -> westOf
      (Pos _,  Pos _) -> southwestOf
      (Pos _, Neg _) -> northwestOf
      (Neg _, Pos _) -> southeastOf
      (Neg _, Neg _) -> northeastOf
      _ -> error "This should never happen. translated smart constructor wasn't used."
orientation _ = ontopOf



sampleSolution :: Drawable a => a
sampleSolution = translated 0 6 (colored yellow (solidCircle 1)) & colored green (solidRectangle 20 2)


example1 :: Drawable a => a
example1 = colored yellow (solidCircle 2) & translated 0 (-5) (colored green (solidRectangle 12 3))

example2 :: Drawable a => a
example2 = translated 0 6 (colored yellow(solidCircle 1)) &
           colored green (solidRectangle 12 2)


example3 :: Drawable a => a
example3 = grass & sun
  where grass = colored green (solidRectangle 20 2)
        sun = translated 0 7 (colored yellow (solidCircle 1.5))


example4 :: Drawable a => a
example4 = ebene & translated 0 9 sonne
  where boden = solidRectangle 20 2
        ebene = colored green boden
        sonne = colored yellow (solidCircle 1)


example5 :: Drawable a => a
example5 = colored yellow (solidCircle 1) &
           translated 0 (-3) (colored green (solidRectangle 8 1.5))


threeCircles :: Drawable a => a
threeCircles =
  colored red (solidCircle 1) &
  translated 2 4 (colored green $ solidCircle 1) &
  translated 0 3 (colored yellow $ solidCircle 1)