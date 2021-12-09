import Data.List (foldl')
import Prelude hiding (concat, foldl, foldl1, foldr, foldr1, map, product, reverse, sum)

-- Folds

sum :: Num a => [a] -> a
sum [] = 0
sum (x : xs) = x + sum xs

product :: Num a => [a] -> a
product [] = 1
product (x : xs) = x * product xs

concat :: [[a]] -> [a]
concat [] = []
concat (x : xs) = x ++ concat xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ acc [] = acc
foldr f acc (x : xs) = f x (foldr f acc xs)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc [] = acc
foldl f acc (x : xs) = foldl f (f acc x) xs

foldrIsRightAssociative = foldr (-) 6 [1, 2, 3, 4] == 1 - (2 - (3 - (4 - 6)))

foldlIsLeftAssociative = foldl (-) 6 [1, 2, 3, 4] == (((6 - 1) - 2) - 3) - 4

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 _ [] = error "empty list"
foldr1 _ [x] = x
foldr1 f (x : xs) = f x (foldr1 f xs)

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 _ [] = error "empty list"
foldl1 f (x : xs) = foldl f x xs

echoes :: [Int] -> [Int]
echoes = foldr (\x xs -> replicate x x ++ xs) []

map :: (a -> b) -> [a] -> [b]
map f = foldr (\x xs -> f x : xs) []

andRecursive, andFold, orRecursive, orFold :: [Bool] -> Bool

andRecursive [] = True
andRecursive (x : xs) = x && andRecursive xs

andFold = foldr (&&) True

orRecursive [] = False
orRecursive (x : xs) = x || orRecursive xs

orFold = foldr (||) False

maximumr, maximuml, minimumr, minimuml :: Ord a => [a] -> a
maximumr = foldr1 max
maximuml = foldl1 max
minimumr = foldr1 min
minimuml = foldl1 min

reverse :: [a] -> [a]
reverse = foldl' (flip (:)) []

-- Scans

scanrRecursive, scanrFold :: (a -> b -> b) -> b -> [a] -> [b]

scanrRecursive _ initial [] = [initial]
scanrRecursive step initial (x : xs) =
  let prev = scanrRecursive step initial xs
   in step x (head prev) : prev

scanrFold step initial = foldr (\x xs -> step x (head xs) : xs) [initial]

scanlRecursive, scanlFold, scanlFoldSlow :: (b -> a -> b) -> b -> [a] -> [b]

scanlRecursive _ initial [] = [initial]
scanlRecursive step initial (x : xs) = initial : scanlRecursive step (step initial x) xs

scanlFold step initial = reverse . foldl (\xs x -> step (head xs) x : xs) [initial]

scanlFoldSlow step initial = foldl (\xs x -> xs ++ [step (last xs) x]) [initial]

factList :: (Num a, Enum a) => a -> [a]
factList n = scanl1 (*) [1 .. n]

-- filter

retainEven, filterEven :: Integral a => [a] -> [a]

retainEven [] = []
retainEven (n : ns) = if n `mod` 2 == 0 then n : retainEven ns else retainEven ns

filterEven = filter even

-- List comprehensions

comprehendEven, comprehendLargeEven, comprehendEvenMinusOne :: Integral a => [a] -> [a]

comprehendEven ns = [n | n <- ns, even n]

comprehendLargeEven ns = [n | n <- ns, even n, n > 100]

comprehendEvenMinusOne ns = [n - 1 | n <- ns, even n]

firstForEvenSeconds :: Integral a => [(b, a)] -> [b]
firstForEvenSeconds ps = [x | (x, y) <- ps, even y]

doubleOfFirstForEvenSeconds :: (Integral a, Num b) => [(b, a)] -> [b]
doubleOfFirstForEvenSeconds ps = [2 * x | (x, y) <- ps, even y]

allPairs :: (Num a, Enum a, Num b, Enum b) => [(a, b)]
allPairs = [(x, y) | x <- [1 .. 4], y <- [5 .. 8]]

somePairs :: (Num a, Enum a, Ord a) => [(a, a)]
somePairs = [(x, y) | x <- [1 .. 4], y <- [5 .. 8], x + y > 8]

returnDivisible, returnDivisibleFilter :: Integral a => a -> [a] -> [a]

returnDivisible d ns = [n | n <- ns, n `mod` d == 0]

returnDivisibleFilter d = filter (\n -> n `mod` d == 0)

choosingTails :: (Num a, Ord a) => [[a]] -> [[a]]
choosingTails ls = [t | (h : t) <- ls, h > 5]

comprehendingFilter :: (a -> Bool) -> [a] -> [a]
comprehendingFilter pred xs = [x | x <- xs, pred x]

comprehendingMap :: (a -> b) -> [a] -> [b]
comprehendingMap f xs = [f x | x <- xs]

doubleOfFirstForEvenSecondsFilterMap :: (Integral a, Num b) => [(b, a)] -> [b]
doubleOfFirstForEvenSecondsFilterMap = map ((2 *) . fst) . filter (even . snd)
