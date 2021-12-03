import Data.List (group)

-- Rebuilding lists

doubleList :: Num a => [a] -> [a]
doubleList [] = []
doubleList (x : xs) = (2 * x) : doubleList xs

tripleList :: Num a => [a] -> [a]
tripleList [] = []
tripleList (x : xs) = (3 * x) : tripleList xs

multiplyList :: Num a => a -> [a] -> [a]
multiplyList _ [] = []
multiplyList x (y : ys) = (x * y) : multiplyList x ys

myTake :: Integral a => a -> [b] -> [b]
myTake _ [] = []
myTake n _ | n < 1 = []
myTake n (x : xs) = x : myTake (n - 1) xs

myDrop :: Integral a => a -> [b] -> [b]
myDrop _ [] = []
myDrop n list | n < 1 = list
myDrop n (x : xs) = myDrop (n - 1) xs

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x : xs) = x + mySum xs

scanSum :: Num a => [a] -> [a]
scanSum [] = []
scanSum [x] = [x]
scanSum (x : x' : xs) = x : scanSum ((x + x') : xs)

diffs :: Num a => [a] -> [a]
diffs [] = []
diffs [_] = []
diffs (x : x' : xs) = (x' - x) : diffs (x' : xs)

-- Generalizing even further

alternativeDoubleList :: Num a => [a] -> [a]
alternativeDoubleList = multiplyList 2

applyToNums :: Num a => (a -> a) -> [a] -> [a]
applyToNums _ [] = []
applyToNums f (x : xs) = f x : applyToNums f xs

alternativeMultiplyList :: Num a => a -> [a] -> [a]
alternativeMultiplyList x = applyToNums ((*) x)

-- The map function

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x : xs) = f x : myMap f xs

anotherAlternativeMultiplyList :: Num a => a -> [a] -> [a]
anotherAlternativeMultiplyList x = myMap ((*) x)

heads :: [[a]] -> [a]
heads = myMap head

negateList :: Num a => [a] -> [a]
negateList = myMap negate

divisors :: Integral a => a -> [a]
divisors p = [f | f <- [1 .. p], p `mod` f == 0]

divisorsList :: Integral a => [a] -> [[a]]
divisorsList = myMap divisors

negateDivisorsList :: Integral a => [a] -> [[a]]
negateDivisorsList = myMap (negateList . divisors)

encode :: String -> [(Int, Char)]
encode decoded = myMap stringToLengthAndChar (group decoded)
  where
    stringToLengthAndChar string = (length string, head string)

decode :: [(Int, Char)] -> String
decode encoded = concat (myMap lenghtAndCharToString encoded)
  where
    -- lenghtAndCharToString (length, char) = replicate length char
    lenghtAndCharToString = uncurry replicate

-- Tips and Tricks

integralsFrom :: Integral a => a -> [a]
integralsFrom n = [n ..]

evens :: Integral a => [a]
evens = doubleList [1 ..]

haskellIsLazy = scanSum (myTake 10 [1 ..]) == myTake 10 (scanSum [1 ..])

myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (_ : xs) = myLast xs

myInit :: [a] -> [a]
myInit [] = error "empty list"
myInit [_] = []
myInit (x : xs) = x : myInit xs
