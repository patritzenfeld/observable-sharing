{-# language MultiParamTypeClasses#-}
{-# language FlexibleContexts #-}

module HashCons (module HashCons) where


import Control.Monad.State
import Data.Text (Text)
import Data.List (elemIndex)

import Picture



type NodeId = Int


data Node
  = NRectangle Double Double
  | NThickRectangle Double Double Double
  | NSolidRectangle Double Double
  | NCircle Double
  | NThickCircle Double Double
  | NSolidCircle Double
  | NLettering Text
  | NColor Color NodeId
  | NTranslate Double Double NodeId
  | NScale Double Double NodeId
  | NDilate Double NodeId
  | NRotate Double NodeId
  | NPictures [NodeId]
  | NAnd NodeId NodeId
  | NCoordinatePlane
  | NBlank
  deriving (Eq,Ord,Show)


newtype DAG = DAG (BiMap Node) deriving Show
newtype Runner = Runner { unRunner :: State DAG NodeId}
type BiMap a = [(NodeId,a)]


instance Drawable Runner where
  coordinatePlane     = Runner $ hashcons   NCoordinatePlane
  blank               = Runner $ hashcons   NBlank
  rectangle x         = Runner . hashcons . NRectangle x
  thickRectangle t x  = Runner . hashcons . NThickRectangle t x
  solidRectangle x    = Runner . hashcons . NSolidRectangle x
  circle              = Runner . hashcons . NCircle
  thickCircle t       = Runner . hashcons . NThickCircle t
  solidCircle         = Runner . hashcons . NSolidCircle
  lettering           = Runner . hashcons . NLettering

  colored c p = Runner $ do
    h <- unRunner p
    hashcons $ NColor c h

  translated x y p = Runner $ do
    h <- unRunner p
    hashcons $ NTranslate x y h

  scaled x y p = Runner $ do
    h <- unRunner p
    hashcons $ NScale x y h

  dilated d p = Runner $ do
    h <- unRunner p
    hashcons $ NDilate d h

  rotated a p = Runner $ do
    h <- unRunner p
    hashcons $ NRotate a h

  pictures ps = Runner $ do
    hs <- mapM unRunner ps
    hashcons $ NPictures hs

  p & q = Runner $ do
    h1 <- unRunner p
    h2 <- unRunner q
    hashcons $ NAnd h1 h2


lookupKey :: Ord a => a -> BiMap a -> Maybe NodeId
lookupKey a xs = elemIndex a $ map snd xs


lookupVal :: NodeId -> BiMap a -> a
lookupVal i xs = snd $ xs !! i


insert :: a -> BiMap a -> (Int, BiMap a)
insert a xs = let i = length xs in (i,xs++[(i,a)])


empty :: [a]
empty = []


run :: Runner -> (NodeId, DAG)
run (Runner m) = runState m (DAG empty)


hashcons :: Node -> State DAG NodeId
hashcons e = do
  DAG m <- get
  case lookupKey e m of
    Nothing ->
      let (k, m') = insert e m
      in put (DAG m') >> return k
    Just k -> return k


hashconsShare :: Runner -> (BiMap Node,BiMap Node)
hashconsShare r = (map (\i -> (i, lookupVal i bimap)) shared, bimap)
  where
    (_, DAG bimap) = run r
    indices = concatMap (getNodes . snd) bimap
    keys = map fst bimap
    count x = length . filter (==x)
    shared = filter ((> 1) . flip count indices) keys


getNodes :: Node -> [NodeId]
getNodes n = case n of
  NColor _ i       -> [i]
  NTranslate _ _ i -> [i]
  NScale _ _ i     -> [i]
  NDilate _ i      -> [i]
  NRotate _ i      -> [i]
  NPictures is     -> is
  NAnd i1 i2       -> [i1,i2]
  _                -> []


test1 :: Drawable a => a
test1 = circle 1 & translated 1 2 (circle 1)


test2 :: Drawable a => a
test2 = let c = circle 1 in c & translated 1 2 c