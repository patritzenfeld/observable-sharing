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
  | ThickClosedCurveNode Double [Point]
  | PolylineNode [Point]
  | ThickPolylineNode [Point] Double
  | CurveNode [Point]
  | ThickCurveNode Double [Point]
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
  coordinatePlane      = toRunnerSimple CoordinatePlaneNode
  blank                = toRunnerSimple BlankNode
  rectangle x          = toRunnerSimple . RectangleNode x
  thickRectangle t x   = toRunnerSimple . ThickRectangleNode t x
  solidRectangle x     = toRunnerSimple . SolidRectangleNode x
  circle               = toRunnerSimple . CircleNode
  thickCircle t        = toRunnerSimple . ThickCircleNode t
  solidCircle          = toRunnerSimple . SolidCircleNode
  arc a1 a2            = toRunnerSimple . ArcNode a1 a2
  sector a1 a2         = toRunnerSimple . SectorNode a1 a2
  thickArc t a1 a2     = toRunnerSimple . ThickArcNode t a1 a2
  curve                = toRunnerSimple . CurveNode
  thickCurve t         = toRunnerSimple . ThickCurveNode t
  closedCurve          = toRunnerSimple . ClosedCurveNode
  thickClosedCurve t   = toRunnerSimple . ThickClosedCurveNode t
  solidClosedCurve     = toRunnerSimple . SolidClosedCurveNode
  lettering            = toRunnerSimple . LetteringNode
  styledLettering ts f = toRunnerSimple . StyledLetteringNode ts f

  colored c      = toRunnerSingle $ ColorNode c
  translated x y = toRunnerSingle $ TranslateNode x y
  scaled x y     = toRunnerSingle $ ScaleNode x y
  dilated d      = toRunnerSingle $ DilateNode d
  rotated a      = toRunnerSingle $ RotateNode a

  pictures ps = Runner $ do
    hs <- mapM unRunner ps
    hashcons $ PicturesNode hs

  p & q = Runner $ do
    h1 <- unRunner p
    h2 <- unRunner q
    hashcons $ AndNode h1 h2


toRunnerSimple :: Node -> Runner
toRunnerSimple = Runner . hashcons


toRunnerSingle :: (NodeId -> Node) -> Runner -> Runner
toRunnerSingle f x = Runner $ do
    hs <- unRunner x
    hashcons $ f hs


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