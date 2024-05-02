{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

module MinimumSpanningTree where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

data HList xs where
  HNil :: HList '[]
  (:#) :: x -> HList xs -> HList (x ': xs)

infixr 5 :#

deriving instance Show (HList '[])

deriving instance (Show x, Show (HList xs)) => Show (HList (x ': xs))

data Edge a w where
  Edge :: a -> a -> Edge a 0
  WeightedEdge :: a -> a -> Edge a w

instance (Show a, KnownNat w) => Show (Edge a w) where
  show (Edge from to) = show from ++ " ---> " ++ show to
  show (WeightedEdge from to) = show from ++ " -" ++ show (natVal (Proxy @w)) ++ "-> " ++ show to
