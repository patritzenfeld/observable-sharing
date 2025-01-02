{-# language RankNTypes #-}
module Compare (module Compare) where


import Data.List.Extra                  ((\\), nubOrd)
import Data.Maybe                       (fromJust)
import Data.Tuple.Extra                 (second, (&&&))
import qualified Data.IntMap            as IM

import API                              (Drawable)
import HashCons                         (BiMap, Node(..), hashconsShare)
import Reify                            (ReifyPicture(..), share)



runShare :: (forall a . Drawable a => a) -> IO ([(IM.Key, ReifyPicture Int)], [(IM.Key, ReifyPicture Int)])
runShare a = do
  (reify,_) <- share a
  let (hCons,_) = hashconsShare a
  let (explicitShares,allShares) = relabel (IM.toList reify) hCons
  if explicitShares == allShares
    then
      putStrLn "You shared everything, good job!"
    else do
      putStrLn "You did not share these subexpressions: "
      print $ allShares \\ explicitShares

  pure (explicitShares,allShares)


relabel :: [(Int,ReifyPicture Int)] -> BiMap Node -> ([(Int,ReifyPicture Int)],[(Int,ReifyPicture Int)])
relabel reifyPic nodes = (reNumber reifiedPic, reNumber reifiedNodes)
  where
    reifiedNodes = map snd $ toReify nodes
    reifiedPic = map snd reifyPic
    mapping = zip (nubOrd $ reifiedPic ++ reifiedNodes) [1..]
    reNumber = map $ (fromJust . flip lookup mapping) &&& id


toReify :: BiMap Node -> [(IM.Key, ReifyPicture Int)]
toReify = map $ second toReifyPic
  where
    toReifyPic n = case n of
      RectangleNode x y -> Rectangle x y
      ThickRectangleNode t x y -> ThickRectangle t x y
      SolidRectangleNode x y -> SolidRectangle x y
      CircleNode r -> Circle r
      ThickCircleNode t r -> ThickCircle t r
      SolidCircleNode r -> SolidCircle r
      PolygonNode ps -> Polygon ps
      SolidPolygonNode ps -> SolidPolygon ps
      ThickPolygonNode ps t -> ThickPolygon ps t
      ClosedCurveNode ps -> ClosedCurve ps
      SolidClosedCurveNode ps -> SolidClosedCurve ps
      ThickClosedCurveNode t ps -> ThickClosedCurve t ps
      PolylineNode ps -> Polyline ps
      ThickPolylineNode ps t -> ThickPolyline ps t
      CurveNode ps -> Curve ps
      ThickCurveNode t ps -> ThickCurve t ps
      SectorNode a1 a2 r -> Sector a1 a2 r
      ArcNode a1 a2 r -> Arc a1 a2 r
      ThickArcNode t a1 a2 r -> ThickArc t a1 a2 r
      LetteringNode t -> Lettering t
      StyledLetteringNode ts f t -> StyledLettering ts f t
      ColorNode c p -> Color c p
      TranslateNode x y p -> Translate x y p
      ScaleNode x y p -> Scale x y p
      DilateNode fac p -> Dilate fac p
      RotateNode a p -> Rotate a p
      ReflectNode a p -> Reflect a p
      ClipNode x y p -> Clip x y p
      PicturesNode ps -> Pictures ps
      AndNode p1 p2 -> And p1 p2
      CoordinatePlaneNode -> CoordinatePlane
      BlankNode -> Blank