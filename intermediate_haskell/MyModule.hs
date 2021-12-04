module MyModule (f, Tree (Branch, Leaf), foldl') where

import Data.List (foldl')

f :: Num a => a -> a
f x = x ^ 2

internalFunction = error "cannot be called from outside"

data Tree a
  = Branch {left, right :: Tree a}
  | Leaf a
