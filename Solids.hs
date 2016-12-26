{-# LANGUAGE DeriveFunctor #-}

module Solids where

-- solid of any dimension described by list of vertices
data Solid p = Solid [p]
  deriving (Show, Functor)

