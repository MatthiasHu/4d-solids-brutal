{-# LANGUAGE DeriveFunctor #-}

module Vectors where

import Control.Applicative


data Vec3 a = Vec3 a a a
  deriving (Show, Eq, Functor)
data Vec4 a = Vec4 a a a a
  deriving (Show, Eq, Functor)

instance Applicative Vec3 where
  pure x = Vec3 x x x
  Vec3 f g h <*> Vec3 x y z = Vec3 (f x) (g y) (h z)
instance Applicative Vec4 where
  pure x = Vec4 x x x x
  Vec4 f g h i <*> Vec4 x y z w = Vec4 (f x) (g y) (h z) (i w)
  

coord4dw :: Vec4 a -> a
coord4dw (Vec4 x y z w) = w

coord4dxyz :: Vec4 a -> Vec3 a
coord4dxyz (Vec4 x y z w) = Vec3 x y z

plus3d :: (Num a) => Vec3 a -> Vec3 a -> Vec3 a
plus3d = liftA2 (+)

plus4d :: (Num a) => Vec4 a -> Vec4 a -> Vec4 a
plus4d = liftA2 (+)

smult3d :: (Num a) => a -> Vec3 a -> Vec3 a
smult3d s = fmap (s*)

smult4d :: (Num a) => a -> Vec4 a -> Vec4 a
smult4d s = fmap (s*)
