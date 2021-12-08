-- Lookups: Data.Map and co.

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq, ViewL (EmptyL, (:<)), ViewR (EmptyR, (:>)), (<|), (><), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

m1 :: Map k v
m1 = Map.empty

m2, reversedM2, m3 :: (Num k, Ord k) => Map k String
m2 = Map.fromList [(1, "Robert"), (5, "Ian"), (6, "Bruce")]
reversedM2 = fmap reverse m2
m3 = Map.union m2 $ Map.fromList [(3, "Andrew"), (17, "Mike")]

sizeM2, sizeM3 :: Int
sizeM2 = Map.size m2
sizeM3 = Map.size m3

ian :: Maybe String
ian = Map.lookup 5 m2

noone :: Maybe String
noone = Map.lookup 7 m2

im1 :: IntMap v
im1 = IntMap.empty

im2 :: IntMap String
im2 = IntMap.fromList [(1, "Robert"), (5, "Ian"), (6, "Bruce")]

keysIm2 :: [IntMap.Key] -- = [Int]
keysIm2 = IntMap.keys im2

s1 :: Set a
s1 = Set.empty

s2 :: Set String
s2 = Set.fromList ["me", "myself", "I"]

-- Peeking at both ends with Data.Sequence

seq1, seq2, seq3, seq4 :: Num a => Seq a
seq1 = Seq.fromList [1, 3, 5, 2, 9]
seq2 = 0 <| seq1
seq3 = seq1 |> 1000
seq4 = seq1 >< seq1

seq1ViewRight :: Num a => (Seq a, a)
seq1ViewRight = (xs, x) where xs :> x = Seq.viewr seq1

seq1ViewLeft :: Num a => (a, Seq a)
seq1ViewLeft = (x, xs) where x :< xs = Seq.viewl seq1

seqFoldr :: (a -> b -> b) -> b -> Seq a -> b
seqFoldr f acc = g . Seq.viewl
  where
    g EmptyL = acc
    g (x :< xs) = f x (seqFoldr f acc xs)

seqFoldl :: (b -> a -> b) -> b -> Seq a -> b
seqFoldl f acc = g . Seq.viewr
  where
    g EmptyR = acc
    g (xs :> x) = f (seqFoldl f acc xs) x
