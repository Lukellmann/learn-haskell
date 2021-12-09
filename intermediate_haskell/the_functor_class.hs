import Prelude hiding (Functor (..))

-- Motivation

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf x) = Leaf (f x)
treeMap f (Branch left right) = Branch (treeMap f left) (treeMap f right)

-- Introducing Functor

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x)

instance Functor [] where
  fmap = map

instance Functor Tree where
  fmap = treeMap
