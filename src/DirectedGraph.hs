{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module DirectedGraph where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

type DirectedEdge :: Type -> Nat -> Type
data DirectedEdge a w where
  WeightedDirectedEdge :: a -> a -> DirectedEdge a w

deriving instance (Show a) => Show (DirectedEdge a w)

type PathCost :: [DirectedEdge a w] -> Natural
type family PathCost xs where
  PathCost '[] = 0
  PathCost (WeightedDirectedEdge _ w : xs) = w + PathCost xs

edge :: a -> a -> DirectedEdge a w
edge = WeightedDirectedEdge

data Node :: Type -> Type where
  Node :: a -> Node a

node :: a -> Node a
node = Node

instance (Show a) => Show (Node a) where
  show (Node a) = show a

-- cost :: forall a w (xs :: [DirectedEdge a w]). KnownNat (PathCost xs) => xs -> Integer
-- cost = natVal (Proxy @(PathCost xs))
