import Data.List (group)
import Prelude hiding (drop, init, last, map, sum, take)

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

take :: Integral a => a -> [b] -> [b]
take _ [] = []
take n _ | n < 1 = []
take n (x : xs) = x : take (n - 1) xs

drop :: Integral a => a -> [b] -> [b]
drop _ [] = []
drop n list | n < 1 = list
drop n (x : xs) = drop (n - 1) xs

sum :: Num a => [a] -> a
sum [] = 0
sum (x : xs) = x + sum xs

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

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs

anotherAlternativeMultiplyList :: Num a => a -> [a] -> [a]
anotherAlternativeMultiplyList x = map ((*) x)

heads :: [[a]] -> [a]
heads = map head

negateList :: Num a => [a] -> [a]
negateList = map negate

divisors :: Integral a => a -> [a]
divisors p = [f | f <- [1 .. p], p `mod` f == 0]

divisorsList :: Integral a => [a] -> [[a]]
divisorsList = map divisors

negateDivisorsList :: Integral a => [a] -> [[a]]
negateDivisorsList = map (negateList . divisors)

encode :: String -> [(Int, Char)]
encode decoded = map stringToLengthAndChar (group decoded)
  where
    stringToLengthAndChar string = (length string, head string)

decode :: [(Int, Char)] -> String
decode encoded = concat (map lenghtAndCharToString encoded)
  where
    -- lenghtAndCharToString (length, char) = replicate length char
    lenghtAndCharToString = uncurry replicate

-- Tips and Tricks

integralsFrom :: Integral a => a -> [a]
integralsFrom n = [n ..]

evens :: Integral a => [a]
evens = doubleList [1 ..]

haskellIsLazy = scanSum (take 10 [1 ..]) == take 10 (scanSum [1 ..])

last :: [a] -> a
last [] = error "empty list"
last [x] = x
last (_ : xs) = last xs

init :: [a] -> [a]
init [] = error "empty list"
init [_] = []
init (x : xs) = x : init xs
