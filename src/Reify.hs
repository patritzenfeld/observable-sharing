{-# language DeriveTraversable #-}
{-# language TypeFamilies #-}

module Reify (module Reify) where


import Data.Foldable                    (toList)
import Data.IntMap                      (IntMap, Key)
import Data.Reify                       (Graph(..), MuRef(..), reifyGraph)
import Data.Text                        (Text)
import qualified Data.IntMap            as IM

import API                              (Drawable(..))
import Types                            (Font, TextStyle, Point, Color)



data ReifyPicture a
  = Rectangle Double Double
  | ThickRectangle Double Double Double
  | SolidRectangle Double Double
  | Circle Double
  | ThickCircle Double Double
  | SolidCircle Double
  | Polygon [Point]
  | SolidPolygon [Point]
  | ThickPolygon [Point] Double
  | Polyline [Point]
  | ThickPolyline [Point] Double
  | Sector Double Double Double
  | Arc Double Double Double
  | ThickArc Double Double Double Double
  | Curve [Point]
  | ThickCurve Double [Point]
  | ClosedCurve [Point]
  | SolidClosedCurve [Point]
  | ThickClosedCurve Double [Point]
  | Lettering Text
  | StyledLettering TextStyle Font Text
  | Color Color a
  | Translate Double Double a
  | Scale Double Double a
  | Dilate Double a
  | Rotate Double a
  | Reflect Double a
  | Clip Double Double a
  | Pictures [a]
  | And a a
  | CoordinatePlane
  | Blank
  deriving (Show, Foldable, Eq, Ord)


newtype PRec a = PRec (ReifyPicture (PRec a))


instance Drawable (PRec a) where
  rectangle x          = PRec . Rectangle x
  thickRectangle t x   = PRec . ThickRectangle t x
  solidRectangle x     = PRec . SolidRectangle x
  circle               = PRec . Circle
  thickCircle t        = PRec . ThickCircle t
  solidCircle          = PRec . SolidCircle
  arc a1 a2            = PRec . Arc a1 a2
  sector a1 a2         = PRec . Sector a1 a2
  thickArc t a1 a2     = PRec . ThickArc t a1 a2
  curve                = PRec . Curve
  thickCurve t         = PRec . ThickCurve t
  closedCurve          = PRec . ClosedCurve
  thickClosedCurve t   = PRec . ThickClosedCurve t
  solidClosedCurve     = PRec . SolidClosedCurve
  lettering            = PRec . Lettering
  styledLettering ts f = PRec . StyledLettering ts f
  colored c            = PRec . Color c
  translated x y       = PRec . Translate x y
  scaled x y           = PRec . Scale x y
  dilated a            = PRec . Dilate a
  rotated a            = PRec . Rotate a
  pictures             = PRec . Pictures
  a & b                = PRec $ And a b
  coordinatePlane      = PRec CoordinatePlane
  blank                = PRec Blank


instance MuRef (PRec a) where
  type DeRef (PRec a) = ReifyPicture
  mapDeRef f (PRec body) = case body of
    Color c p       -> Color c <$> f p
    Translate x y p -> Translate x y <$> f p
    Scale x y p     -> Scale x y <$> f p
    Dilate x p      -> Dilate x <$> f p
    Rotate a p      -> Rotate a <$> f p
    Reflect a p     -> Reflect a <$> f p
    Clip x y p      -> Clip x y <$> f p
    And a b         -> And <$> f a <*> f b
    Pictures ps     -> Pictures <$> traverse f ps
    _               -> pure $ changeBaseType body


changeBaseType :: ReifyPicture a1 -> ReifyPicture a2
changeBaseType p = case p of
  Rectangle x y         -> Rectangle x y
  ThickRectangle t x y  -> ThickRectangle t x y
  SolidRectangle x y    -> SolidRectangle x y
  Circle r              -> Circle r
  ThickCircle t r       -> ThickCircle t r
  SolidCircle r         -> SolidCircle r
  Lettering t           -> Lettering t
  StyledLettering s w t -> StyledLettering s w t
  Curve xs              -> Curve xs
  ThickCurve t xs       -> ThickCurve t xs
  ClosedCurve xs        -> ClosedCurve xs
  SolidClosedCurve xs   -> SolidClosedCurve xs
  ThickClosedCurve xs t -> ThickClosedCurve xs t
  Polygon xs            -> Polygon xs
  SolidPolygon xs       -> SolidPolygon xs
  ThickPolygon xs t     -> ThickPolygon xs t
  Polyline xs           -> Polyline xs
  ThickPolyline xs t    -> ThickPolyline xs t
  Sector a1 a2 r        -> Sector a1 a2 r
  Arc a1 a2 r           -> Arc a1 a2 r
  ThickArc t a1 a2 r    -> ThickArc t a1 a2 r
  CoordinatePlane       -> CoordinatePlane
  Blank                 -> Blank
  _                     -> error "This is a recursive Constructor. You're missing a pattern match!"


share :: PRec a -> IO (IntMap (ReifyPicture Int), IntMap (ReifyPicture Int))
share d = do
  Graph nodes s <- reifyGraph d
  let universe = IM.fromList nodes
      refs = IM.insertWith (+) s 1 $ foldr (mapInsertWith . toList . snd) mempty nodes
      multiRefs = IM.intersection universe $ IM.filter (>1) refs
      lut = IM.intersection universe refs
  pure (multiRefs, lut)


mapInsertWith :: [Key] -> IntMap Int -> IntMap Int
mapInsertWith b m = Prelude.foldr (\x acc-> IM.insertWith (+) x (1 :: Int) acc) m b


test :: Drawable a => a
test = let r = rectangle 2 3 in r & r
