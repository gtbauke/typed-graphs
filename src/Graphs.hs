{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphs where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

type family If (c :: Bool) (t :: k) (f :: k) :: k where
  If 'True t f = t
  If 'False t f = f

type family (||) (a :: Bool) (b :: Bool) :: Bool where
  'True || b = 'True
  'False || b = b

type family Not (a :: Bool) :: Bool where
  Not 'True = 'False
  Not 'False = 'True

type family AsConstraint (c :: Bool) :: Constraint where
  AsConstraint c = c ~ 'True

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

type HListElem :: k -> xs -> Bool
type family HListElem x xs where
  HListElem x Nil = 'False
  HListElem x (x :# xs) = 'True
  HListElem x (y :# xs) = HListElem x xs

type HListUniqueElements :: HList xs -> HList ys
type family HListUniqueElements xs where
  HListUniqueElements Nil = Nil
  HListUniqueElements (x :# xs) = If (HListElem x xs) (HListUniqueElements xs) (x :# HListUniqueElements xs)

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

type HasCycleStartingFromInternal :: Symbol -> xs -> visited -> Bool
type family HasCycleStartingFromInternal start xs visited where
  HasCycleStartingFromInternal start Nil visited = 'False
  HasCycleStartingFromInternal start (Edge start next _ :# xs) visited = 'True
  HasCycleStartingFromInternal start (Edge from to _ :# xs) visited = HasCycleStartingFromInternal start xs (to :# visited)

type HasCycleStartingFrom :: Symbol -> xs -> Bool
type family HasCycleStartingFrom start xs where
  HasCycleStartingFrom start xs = HasCycleStartingFromInternal start xs Nil

type UniqueNodes :: xs -> ys
type family UniqueNodes xs where
  UniqueNodes (Edge from to _ :# xs') = from :# UniqueNodes xs'

type HasCycle :: xs -> Bool
type family HasCycle xs where
  HasCycle Nil = 'False
  HasCycle (x :# xs) = HasCycleStartingFrom x (x :# xs) || HasCycle xs

type family AcyclicBool xs where
  AcyclicBool xs = Not (HasCycle xs)

type family Acyclic xs where
  Acyclic xs = AsConstraint (AcyclicBool xs)

type family AcyclicAsNat xs where
  AcyclicAsNat xs = If (AcyclicBool xs) 1 0

mustBeAcyclic :: (AcyclicAsNat edges ~ 1) => HList edges -> ()
mustBeAcyclic _ = ()
