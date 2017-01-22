{-# LANGUAGE DeriveFunctor #-}

module Solids where

-- solid of any dimension described by list of vertices
data Solid p = Solid [p]
  deriving (Show, Functor)

data Edge p = Edge p p
  deriving (Eq, Ord, Show, Functor)
