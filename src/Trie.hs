{-# language OverloadedStrings #-}

module Trie (module Trie) where


import Data.ByteString (ByteString)
import Data.List (intersperse)
import qualified Data.ByteString as BS
import Data.Trie (Trie)
import qualified Data.Trie as T
import Control.Monad.State
import HashCons (Node(..))
import Picture (Drawable(..))

type NodeId = Int

data DAG = DAG {
  unTrie :: Trie (Node,NodeId),
  maximumId :: NodeId
} deriving Show


data Graph = Graph {
  unGraph :: State DAG NodeId,
  stringAST :: ByteString
  }


instance Drawable Graph where
  rectangle x y =
    let
      node = RectangleNode x y
      sAST = buildStringAST node []
    in
      Graph (triecons sAST $ RectangleNode x y) sAST

  solidRectangle x y =
    let
      node = SolidRectangleNode x y
      sAST = buildStringAST node []
    in
      Graph (triecons sAST $ SolidRectangleNode x y) sAST

  thickRectangle t x y =
    let
      node = ThickRectangleNode t x y
      sAST = buildStringAST node []
    in
      Graph (triecons sAST $ ThickRectangleNode t x y) sAST

  circle r =
    let
      node = CircleNode r
      sAST = buildStringAST node []
    in
      Graph (triecons sAST $ CircleNode r) sAST

  solidCircle r =
    let
      node = SolidCircleNode r
      sAST = buildStringAST node []
    in
      Graph (triecons sAST $ SolidCircleNode r) sAST

  thickCircle t r =
    let
      node = ThickCircleNode t r
      sAST = buildStringAST node []
    in
      Graph (triecons sAST $ ThickCircleNode t r) sAST

  lettering t =
    let
      node = LetteringNode t
      sAST = buildStringAST node []
    in
      Graph (triecons sAST $ LetteringNode t) sAST

  translated x y p =
    let
      sAST = buildStringAST p "translated"
      sT = do
        n <- seqArgs [p]
        case n of
          [e] -> triecons sAST $ TranslateNode x y e
          _ -> error "no"
    in
      Graph sT sAST

{-
constant x = let
node = NConstant x
sAST = buildStringAST node []
in Graph (triecons sAST $ NConstant x) sAST
variable x = let
node = NVariable x
sAST = buildStringAST node []
in Graph (triecons sAST $ NVariable x) sAST
add e1 e2 = let
sAST = buildStringAST "nadd" [e1,e2]
sT = do ns <- seqArgs [e1,e2]
case ns of
[n1,n2] -> triecons sAST $ NAdd n1 n2
_ -> error "black magic"
in Graph sT sAST
-}

triecons :: ByteString -> Node -> State DAG NodeId
triecons sAST node = do
  DAG trie maxId <- get
  case T.lookup sAST trie of
    Nothing -> do
      let
        maxId' = maxId+1
        trie' = T.insert sAST (node,maxId') trie
      put $ DAG trie' maxId'
      pure maxId'
    Just (_,nodeId) -> pure nodeId


seqArgs :: [Graph] -> State DAG [NodeId]
seqArgs = mapM seqArg
  where
    seqArg (Graph sT sAST) = do
      DAG trie _ <- get
      case T.lookup sAST trie of
        Nothing -> sT
        Just (_,nodeID) -> pure nodeID


buildStringAST :: Node -> [ByteString] -> ByteString
buildStringAST node args = opString <> argsString
  where
    opString = case node of
      RectangleNode _ _ -> "rectangle"
      ThickRectangleNode t x y -> "thickRectangle"
      SolidRectangleNode x y -> "solidRectangle"
      CircleNode r -> "circle"
      ThickCircleNode t r -> "thickCircle"
      SolidCircleNode r -> "solidCircle"
      LetteringNode t -> "lettering"
      ColorNode c n -> "color"
      TranslateNode x y n -> "translated"
      ScaleNode x y n -> "scaled"
      DilateNode d n -> "dilated"
      RotateNode a n -> "rotated"
      PicturesNode ns -> "pictures"
      AndNode n1 n2 -> "(&)"
      CoordinatePlaneNode -> "coordinatePlane"
      _ -> "blank"
    argsString = "(" <> BS.concat (intersperse "," args) <> ")"
