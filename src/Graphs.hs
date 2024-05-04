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

-- type CountNeighbors :: xs -> Nat
-- type family CountNeighbors ns where
--   CountNeighbors Nil = 0
--   CountNeighbors (g :# gs) = 1 + CountNeighbors gs

infixr 5 :#

instance (Every Show xs) => Show (HList xs) where
  show Nil = ""
  show (g :# gs) = show g ++ ", " ++ show gs

-- type Graph :: Type -> Natural -> Type
-- data Graph a n where
--   Graph :: a -> HList xs -> Graph a (CountNeighbors xs)

-- type family If (b :: Bool) (t :: k) (f :: k) :: k where
--   If 'True t f = t
--   If 'False t f = f

-- type family (==) (a :: k) (b :: k) :: Bool where
--   a == a = 'True
--   a == b = 'False

-- type family IsEven (n :: Nat) :: Constraint where
--   IsEven n = If (Mod n 2 == 0) (() :: Constraint) (TypeError ('Text "Expected an even number"))

-- type HasEulerianPath :: xs -> Constraint
-- type family HasEulerianPath xs where
--   HasEulerianPath Nil = ()
--   HasEulerianPath (Graph a n :# gs) = (IsEven n, HasEulerianPath gs)

-- type family HasEulerianPathBool xs where
--   HasEulerianPathBool xs = HasEulerianPath xs ~ (() :: Constraint)

type Edge :: Symbol -> Symbol -> Type
data Edge from to = Edge

instance (KnownSymbol from, KnownSymbol to) => Show (Edge from to) where
  show _ = symbolVal (Proxy @from) ++ " -> " ++ symbolVal (Proxy @to)

edges = Edge @"a" @"b" :# Edge @"b" @"c" :# Edge @"c" @"a" :# Nil
