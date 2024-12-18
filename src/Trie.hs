{-# language OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Trie (
  module Trie
  ) where


import Control.Monad.Identity           (Identity)
import Control.Monad.State              (State, get, put, runState)
import Data.ByteString                  (ByteString)
import Data.List                        (intersperse)
import Data.Text.Encoding               (encodeUtf8)
import Data.Trie                        (Trie)
import qualified Data.ByteString.Char8  as BS
import qualified Data.Trie              as T

import HashCons                         (Node(..), getNodes)
import Picture                          (Drawable(..))
import Types                            (NodeId)



data DAG = DAG {
  unTrie :: Trie (NodeId,Node),
  maximumId :: NodeId
} deriving Show


data Graph = Graph {
  unGraph :: State DAG NodeId,
  unStringAST :: ByteString
  }


instance MonadFail Identity where
  fail = error "Computation Didn't return expected amount of arguments."



instance Drawable Graph where

  blank = processSimple BlankNode

  coordinatePlane = processSimple CoordinatePlaneNode

  rectangle x = processSimple . RectangleNode x

  solidRectangle x = processSimple . SolidRectangleNode x

  thickRectangle t x = processSimple . ThickRectangleNode t x

  circle = processSimple . CircleNode

  solidCircle = processSimple. SolidCircleNode

  thickCircle t = processSimple. ThickCircleNode t

  lettering = processSimple . LetteringNode

  translated x y = processOneGraph $ TranslateNode x y

  colored c = processOneGraph $ ColorNode c

  dilated d = processOneGraph $ DilateNode d

  scaled x y = processOneGraph $ ScaleNode x y

  rotated a = processOneGraph $ RotateNode a

  pictures ps = Graph sT sAST
    where
      sAST = buildStringAST (PicturesNode undefined) $ map unStringAST ps
      sT = do
        n <- seqArgs ps
        triecons sAST $ PicturesNode n

  p & q = Graph sT sAST
    where
      sAST = buildStringAST (AndNode undefined undefined) $ map unStringAST [p,q]
      sT = do
        [e1,e2] <- seqArgs [p,q]
        triecons sAST $ AndNode e1 e2


processSimple :: Node -> Graph
processSimple node = Graph (triecons sAST node) sAST
  where
    sAST = buildStringAST node []


processOneGraph :: (NodeId -> Node) -> Graph -> Graph
processOneGraph partialNode graph = Graph sT sAST
    where
      sAST = buildStringAST (partialNode undefined) [unStringAST graph]
      sT = do
        [e] <- seqArgs [graph]
        triecons sAST $ partialNode e


triecons :: ByteString -> Node -> State DAG NodeId
triecons sAST node = do
  DAG trie maxId <- get
  case T.lookup sAST trie of
    Nothing -> do
      let
        maxId' = maxId+1
        trie' = T.insert sAST (maxId',node) trie
      put $ DAG trie' maxId'
      pure maxId'
    Just (nodeId,_) -> pure nodeId


seqArgs :: [Graph] -> State DAG [NodeId]
seqArgs = mapM seqArg
  where
    seqArg (Graph sT sAST) = do
      DAG trie _ <- get
      case T.lookup sAST trie of
        Nothing -> sT
        Just (nodeID,_) -> pure nodeID


buildStringAST :: Node -> [ByteString] -> ByteString
buildStringAST node args = opString <> argsString
  where
    toBS :: Show a => a -> ByteString
    toBS = BS.pack . show

    opString = case node of
      RectangleNode x y -> "rectangle " <> toBS x <> " " <> toBS y
      ThickRectangleNode t x y -> "thickRectangle " <> toBS t <> " " <> toBS x <> " " <> toBS y
      SolidRectangleNode x y -> "solidRectangle " <> toBS x <> " " <> toBS y
      CircleNode r -> "circle " <> toBS r
      ThickCircleNode t r -> "thickCircle " <> toBS t <> " " <> toBS r
      SolidCircleNode r -> "solidCircle " <> toBS r
      LetteringNode t -> "lettering " <> encodeUtf8 t
      ColorNode c _ -> "color " <> toBS c
      TranslateNode x y _ -> "translated " <> toBS x <> " " <> toBS y
      ScaleNode x y _ -> "scaled " <> toBS x <> " " <> toBS y
      DilateNode d _ -> "dilated " <> toBS d
      RotateNode a _ -> "rotated " <> toBS a
      PicturesNode _ -> "pictures"
      AndNode _ _ -> "(&)"
      CoordinatePlaneNode -> "coordinatePlane"
      _ -> "blank"
    argsString = case args of
      [] -> ""
      _  -> " (" <> BS.concat (intersperse ") (" args) <> ")"


buildDAG :: Graph -> (NodeId, DAG)
buildDAG g = runState (unGraph g) (DAG T.empty 0)


shareGraph :: Graph -> ([Node], [(NodeId, Node)])
shareGraph g = (map shared multi, nodeIdPairs)
  where
    DAG trie mId = snd $ buildDAG g
    nodeIdPairs = T.elems trie
    nodeIds = concatMap (getNodes . snd) nodeIdPairs
    count x = length . filter (==x)
    multi = filter ((> 1) . flip count nodeIds) [1..mId]
    shared x = case lookup x nodeIdPairs of
      Just r -> r
      _      -> error "impossible"