import Data.List (foldl')

-- Folds

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x : xs) = x + mySum xs

myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (x : xs) = x * myProduct xs

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x : xs) = x ++ myConcat xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ acc [] = acc
myFoldr f acc (x : xs) = f x (myFoldr f acc xs)

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc [] = acc
myFoldl f acc (x : xs) = myFoldl f (f acc x) xs

foldrIsRightAssociative = myFoldr (-) 6 [1, 2, 3, 4] == 1 - (2 - (3 - (4 - 6)))

foldlIsLeftAssociative = myFoldl (-) 6 [1, 2, 3, 4] == (((6 - 1) - 2) - 3) - 4

myFoldr1 :: (a -> a -> a) -> [a] -> a
myFoldr1 _ [] = error "empty list"
myFoldr1 _ [x] = x
myFoldr1 f (x : xs) = f x (myFoldr1 f xs)

myFoldl1 :: (a -> a -> a) -> [a] -> a
myFoldl1 _ [] = error "empty list"
myFoldl1 f (x : xs) = myFoldl f x xs

echoes :: [Int] -> [Int]
echoes = myFoldr (\x xs -> replicate x x ++ xs) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldr (\x xs -> f x : xs) []

andRecursive, andFold, orRecursive, orFold :: [Bool] -> Bool

andRecursive [] = True
andRecursive (x : xs) = x && andRecursive xs

andFold = myFoldr (&&) True

orRecursive [] = False
orRecursive (x : xs) = x || orRecursive xs

orFold = myFoldr (||) False

myMaximumr, myMaximuml, myMinimumr, myMinimuml :: Ord a => [a] -> a
myMaximumr = myFoldr1 max
myMaximuml = myFoldl1 max
myMinimumr = myFoldr1 min
myMinimuml = myFoldl1 min

myReverse :: [a] -> [a]
myReverse = foldl' (flip (:)) []

-- Scans

myScanrRecursive, myScanrFold :: (a -> b -> b) -> b -> [a] -> [b]

myScanrRecursive _ initial [] = [initial]
myScanrRecursive step initial (x : xs) =
  let prev = myScanrRecursive step initial xs
   in step x (head prev) : prev

myScanrFold step initial = myFoldr (\x xs -> step x (head xs) : xs) [initial]

myScanlRecursive, myScanlFold, myScanlFoldSlow :: (b -> a -> b) -> b -> [a] -> [b]

myScanlRecursive _ initial [] = [initial]
myScanlRecursive step initial (x : xs) = initial : myScanlRecursive step (step initial x) xs

myScanlFold step initial = reverse . myFoldl (\xs x -> step (head xs) x : xs) [initial]

myScanlFoldSlow step initial = myFoldl (\xs x -> xs ++ [step (last xs) x]) [initial]

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

myComprehendingFilter :: (a -> Bool) -> [a] -> [a]
myComprehendingFilter pred xs = [x | x <- xs, pred x]

myComprehendingMap :: (a -> b) -> [a] -> [b]
myComprehendingMap f xs = [f x | x <- xs]

doubleOfFirstForEvenSecondsFilterMap :: (Integral a, Num b) => [(b, a)] -> [b]
doubleOfFirstForEvenSecondsFilterMap = map ((2 *) . fst) . filter (even . snd)
