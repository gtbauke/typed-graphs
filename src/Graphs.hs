module Graphs (DirectedEdge (..), Graph (..), (-->), vertices, adjacent, Vertex (..)) where

import Data.List (nub)

newtype Vertex a = Vertex a deriving (Show, Eq)

data DirectedEdge a = DirectedEdge {from :: Vertex a, to :: Vertex a} deriving (Show, Eq)

(-->) :: a -> a -> DirectedEdge a
a --> b = DirectedEdge {from = Vertex a, to = Vertex b}

newtype Graph a = Graph [DirectedEdge a] deriving (Eq)

instance (Show a) => Show (Graph a) where
  show (Graph edges) = unlines $ map show edges

vertices :: (Eq a) => Graph a -> [Vertex a]
vertices (Graph edges) = nub $ map from edges ++ map to edges

adjacent :: (Eq a) => Graph a -> Vertex a -> [Vertex a]
adjacent (Graph edges) vertex =
  [to edge | edge <- edges, from edge == vertex]
    ++ [from edge | edge <- edges, to edge == vertex]
