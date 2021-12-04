import Data.Char (toLower)

-- A sorting algorithm

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x : xs) = quickSort less ++ (x : equal) ++ quickSort more
  where
    less = filter (< x) xs
    equal = filter (== x) xs
    more = filter (> x) xs

-- Choosing how to compare

dictionary :: [String]
dictionary = words "I have a thing for Linux"

quickSort' :: (a -> a -> Ordering) -> [a] -> [a]
quickSort' _ [] = []
quickSort' compare (x : xs) = quickSort' compare less ++ (x : equal) ++ quickSort' compare more
  where
    cmpx = flip compare x
    less = filter ((LT ==) . cmpx) xs
    equal = filter ((EQ ==) . cmpx) xs
    more = filter ((GT ==) . cmpx) xs

usual :: Ord a => a -> a -> Ordering
usual = compare

descending :: Ord a => a -> a -> Ordering
descending = flip usual

insensitive :: String -> String -> Ordering
insensitive a b = usual (map toLower a) (map toLower b)

-- Higher-Order Functions and Types

for :: a -> (a -> Bool) -> (a -> a) -> (a -> IO ()) -> IO ()
for i cond f job =
  if cond i
    then do
      job i
      for (f i) cond f job
    else return ()

sequenceIO :: [IO a] -> IO [a]
sequenceIO [] = return []
sequenceIO (action : actions) = do
  result <- action
  results <- sequenceIO actions
  return (result : results)

mapIO, mapIO' :: (a -> IO b) -> [a] -> IO [b]
mapIO _ [] = return []
mapIO f (x : xs) = do
  result <- f x
  results <- mapIO f xs
  return (result : results)
mapIO' f list = sequenceIO (map f list)

-- Function manipulation

myFlip :: (a -> b -> c) -> (b -> a -> c) -- same as (a -> b -> c) -> b -> a -> c
myFlip f a b = f b a

o {- my (.) -} :: (b -> c) -> (a -> b) -> (a -> c) -- same as (b -> c) -> (a -> b) -> a -> c
f `o` g = \x -> f (g x)

myInits :: [a] -> [[a]]
myInits = map reverse . scanl (flip (:)) []

($$) {- my ($) -} :: (a -> b) -> a -> b
f $$ x = f x

myInits' :: [a] -> [[a]]
myInits' xs = map reverse . scanl (flip (:)) [] $ xs

mapResult = map ($ 2) [(2 *), (4 *), (8 *)]

myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f x y = f (x, y) -- or myCurry f = \x y -> f (x, y)

myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f (x, y) = f x y -- or myUncurry f = \(x, y) -> f x y

myId :: a -> a
myId x = x

myConst :: a -> (b -> a) -- same as a -> b -> a
myConst x _ = x -- or myConst x = \_ -> x

boringFoldl, coolFoldl, coolFoldl' :: (b -> a -> b) -> b -> [a] -> b
boringFoldl f acc = foldr (flip f) acc . reverse
coolFoldl f acc xs = foldr (flip (.)) id (map (flip f) xs) acc
coolFoldl' f acc = ($ acc) . foldr (flip (.)) id . map (flip f)
