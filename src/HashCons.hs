{-# language MultiParamTypeClasses#-}
{-# language FlexibleContexts #-}

module HashCons (module HashCons) where


import Control.Monad.State              (State, get, put, runState)
import Data.List                        (elemIndex)
import Data.Text                        (Text)

import API                              (Drawable(..))
import Types                            (Color, Font, NodeId, Point, TextStyle)



data Node
  = RectangleNode Double Double
  | ThickRectangleNode Double Double Double
  | SolidRectangleNode Double Double
  | CircleNode Double
  | ThickCircleNode Double Double
  | SolidCircleNode Double
  | PolygonNode [Point]
  | SolidPolygonNode [Point]
  | ThickPolygonNode [Point] Double
  | ClosedCurveNode [Point]
  | SolidClosedCurveNode [Point]
  | ThickClosedCurveNode [Point] Double
  | PolylineNode [Point]
  | ThickPolylineNode [Point] Double
  | CurveNode [Point] Double
  | SectorNode Double Double Double
  | ArcNode Double Double Double
  | ThickArcNode Double Double Double Double
  | LetteringNode Text
  | StyledLetteringNode TextStyle Font Text
  | ColorNode Color NodeId
  | TranslateNode Double Double NodeId
  | ScaleNode Double Double NodeId
  | DilateNode Double NodeId
  | RotateNode Double NodeId
  | ReflectNode Double NodeId
  | ClipNode Double Double NodeId
  | PicturesNode [NodeId]
  | AndNode NodeId NodeId
  | CoordinatePlaneNode
  | BlankNode
  deriving (Eq,Ord,Show)


newtype DAG = DAG (BiMap Node) deriving Show
newtype Runner = Runner { unRunner :: State DAG NodeId}
type BiMap a = [(NodeId,a)]


instance Drawable Runner where
  coordinatePlane     = Runner $ hashcons   CoordinatePlaneNode
  blank               = Runner $ hashcons   BlankNode
  rectangle x         = Runner . hashcons . RectangleNode x
  thickRectangle t x  = Runner . hashcons . ThickRectangleNode t x
  solidRectangle x    = Runner . hashcons . SolidRectangleNode x
  circle              = Runner . hashcons . CircleNode
  thickCircle t       = Runner . hashcons . ThickCircleNode t
  solidCircle         = Runner . hashcons . SolidCircleNode
  lettering           = Runner . hashcons . LetteringNode

  colored c p = Runner $ do
    h <- unRunner p
    hashcons $ ColorNode c h

  translated x y p = Runner $ do
    h <- unRunner p
    hashcons $ TranslateNode x y h

  scaled x y p = Runner $ do
    h <- unRunner p
    hashcons $ ScaleNode x y h

  dilated d p = Runner $ do
    h <- unRunner p
    hashcons $ DilateNode d h

  rotated a p = Runner $ do
    h <- unRunner p
    hashcons $ RotateNode a h

  pictures ps = Runner $ do
    hs <- mapM unRunner ps
    hashcons $ PicturesNode hs

  p & q = Runner $ do
    h1 <- unRunner p
    h2 <- unRunner q
    hashcons $ AndNode h1 h2


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
  ColorNode _ i       -> [i]
  TranslateNode _ _ i -> [i]
  ScaleNode _ _ i     -> [i]
  DilateNode _ i      -> [i]
  RotateNode _ i      -> [i]
  ReflectNode _ i     -> [i]
  ClipNode _ _ i      -> [i]
  PicturesNode is     -> is
  AndNode i1 i2       -> [i1,i2]
  _                   -> []


test1 :: Drawable a => a
test1 = circle 1 & translated 1 2 (circle 1)


test2 :: Drawable a => a
test2 = let c = circle 1 in c & translated 1 2 c