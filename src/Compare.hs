{-# language RankNTypes #-}

module Compare (module Compare) where


import Data.Char                        (toLower)
import Data.List.Extra                  ((\\), nubOrd)
import Data.Maybe                       (fromJust)
import Data.Tuple.Extra                 ((&&&), second)
import qualified Data.IntMap            as IM

import API                              (Drawable)
import HashCons                         (BiMap, Node(..), hashconsShare)
import Reify                            (ReifyPicture(..), share)



runShare :: (forall a . Drawable a => a) -> IO ([(IM.Key, ReifyPicture Int)], [(IM.Key, ReifyPicture Int)])
runShare a = do
  (reify,reifyTerm) <- share a
  let (hCons,_) = hashconsShare a
  let (explicitShares,allShares) = relabel (IM.toList reify) hCons
  let allTerms = IM.toList reifyTerm
  if explicitShares == allShares
    then
      putStrLn "You shared everything, good job!"
    else do
      putStrLn "You did not share these subexpressions: "
      let notShared = allShares \\ explicitShares
      putStrLn $ unlines $ map (flip printOriginal allTerms . snd) notShared
  pure (explicitShares,allShares)



printOriginal :: (Show a, Eq a) => ReifyPicture a -> [(a, ReifyPicture a)] -> String
printOriginal term subTerms = sub term
  where
    getExpr = fromJust . flip lookup subTerms
    recursively n
      | hasArguments expr = '(': printOriginal expr subTerms ++ ")"
      | otherwise = printOriginal expr subTerms
      where
        expr = getExpr n

    sub p = unwords $ case term of
      Color c i       -> ["colored", show c, recursively i]
      Translate x y i -> ["translated", show x, show y, recursively i]
      Scale x y i     -> ["scaled", show x, show y, recursively i]
      Dilate fac i    -> ["dilated", show fac, recursively i]
      Rotate a i      -> ["rotated", show a, recursively i]
      Reflect a i     -> ["reflected", show a, recursively i]
      Clip x y i      -> ["clipped", show x, show y, recursively i]
      Pictures is     -> ["pictures", concatMap recursively is]
      And i1 i2       -> [recursively i1, "&", recursively i2]
      _               -> case show p of
        (x:xs) -> [toLower x:xs]
        _      -> error "not possible"


hasArguments :: ReifyPicture a -> Bool
hasArguments Blank           = False
hasArguments CoordinatePlane = False
hasArguments _               = True


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