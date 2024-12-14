{-# language DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# language AllowAmbiguousTypes #-}

module Reify (module Reify) where

import Data.Text (Text)
import Picture (Drawable(..), Color(..))
import Data.Reify
import qualified Data.IntMap as IM
import Data.IntMap (Key,IntMap)
import Data.Foldable (toList)



data ReifyPicture a
  = Rectangle Double Double
  | ThickRectangle Double Double Double
  | SolidRectangle Double Double
  | Circle Double
  | ThickCircle Double Double
  | SolidCircle Double
  | Lettering Text
  | Color Color a
  | Translate Double Double a
  | Scale Double Double a
  | Dilate Double a
  | Rotate Double a
  | Pictures [a]
  | And a a
  | CoordinatePlane
  | Blank
  deriving (Show, Foldable)


newtype PRec a = PRec (ReifyPicture (PRec a))


instance Drawable (PRec a) where
  rectangle x        = PRec . Rectangle x
  thickRectangle t x = PRec . ThickRectangle t x
  solidRectangle x   = PRec . SolidRectangle x
  circle             = PRec . Circle
  thickCircle t      = PRec . ThickCircle t
  solidCircle        = PRec . SolidCircle
  lettering          = PRec . Lettering
  colored c          = PRec . Color c
  translated x y     = PRec . Translate x y
  scaled x y         = PRec . Scale x y
  dilated a          = PRec . Dilate a
  rotated a          = PRec . Rotate a
  pictures           = PRec . Pictures
  a & b              = PRec $ And a b
  coordinatePlane    = PRec CoordinatePlane
  blank              = PRec Blank


instance MuRef (PRec a) where
  type DeRef (PRec a) = ReifyPicture
  mapDeRef f (PRec body) = case body of
    Color c p            -> Color c <$> f p
    Translate x y p      -> Translate x y <$> f p
    Scale x y p          -> Scale x y <$> f p
    Dilate x p           -> Dilate x <$> f p
    Rotate a p           -> Rotate a <$> f p
    And a b              -> And <$> f a <*> f b
    Pictures ps          -> Pictures <$> traverse f ps
    CoordinatePlane      -> pure CoordinatePlane
    Blank                -> pure Blank
    Rectangle x y        -> pure $ Rectangle x y
    ThickRectangle t x y -> pure $ ThickRectangle t x y
    SolidRectangle x y   -> pure $ SolidRectangle x y
    Circle r             -> pure $ Circle r
    ThickCircle t r      -> pure $ ThickCircle t r
    SolidCircle r        -> pure $ SolidCircle r
    Lettering t          -> pure $ Lettering t


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