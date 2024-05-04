{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphs where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

type family Every (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  Every c '[] = ()
  Every c (x ': xs) = (c x, Every c xs)

data HList xs where
  Nil :: HList '[]
  (:#) :: x -> HList xs -> HList (x ': xs)

infixr 5 :#

instance (Every Show xs) => Show (HList xs) where
  show Nil = ""
  show (g :# gs) = show g ++ ", " ++ show gs

type Edge :: Symbol -> Symbol -> Nat -> Type
data Edge from to weight where
  Edge :: Edge from to 0
  WeightedEdge :: Edge from to weight

instance (KnownSymbol from, KnownSymbol to, KnownNat weight) => Show (Edge from to weight) where
  show _ = fromStr ++ " -" ++ wStr ++ "-> " ++ toStr
    where
      w = natVal (Proxy @weight)
      wStr = if w /= 0 then show w else ""

      fromStr = symbolVal (Proxy @from)
      toStr = symbolVal (Proxy @to)

edges = Edge @"a" @"b" :# Edge @"b" @"c" :# Edge @"c" @"a" :# Nil

weightedEdges = WeightedEdge @"a" @"b" @3 :# WeightedEdge @"b" @"c" @4 :# WeightedEdge @"c" @"a" @5 :# Nil
