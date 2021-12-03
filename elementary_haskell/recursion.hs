import GHC.Natural (Natural)

-- Numeric recursion

factorial :: Natural -> Natural
factorial 0 = 1
factorial n = n * factorial (n - 1)

doubleFactorial :: Natural -> Natural
doubleFactorial 0 = 1
doubleFactorial 1 = 1
doubleFactorial n = n * doubleFactorial (n - 2)

factorialTranslatedFromLoop :: Natural -> Natural
factorialTranslatedFromLoop n = loop n 1
  where
    loop n res
      | n > 1 = loop (n - 1) (res * n)
      | otherwise = res

power :: Num a => a -> Natural -> a
power _ 0 = 1
power x y = x * power x (y - 1)

plusOne :: Num a => a -> a
plusOne x = x + 1

addition :: Num a => a -> Natural -> a
addition x 0 = x
addition x y = addition (plusOne x) (y - 1)

log2 :: Natural -> Natural
log2 1 = 0
log2 n = 1 + log2 (div n 2)

-- List-based recursion

myLength :: [a] -> Natural
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

(+++) :: [a] -> [a] -> [a]
[] +++ ys = ys
(x : xs) +++ ys = x : (xs +++ ys)

myReplicate :: Integral a => a -> b -> [b]
myReplicate n x
  | n < 1 = []
  | otherwise = x : myReplicate (n - 1) x

(!!!) :: [a] -> Natural -> a
[] !!! _ = error "index too large"
[x] !!! 0 = x
(_ : xs) !!! n = xs !!! (n - 1)

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x : xs) (y : ys) = (x, y) : myZip xs ys

myOtherLength :: [a] -> Natural
myOtherLength xs = loop xs 0
  where
    loop [] acc = acc
    loop (_ : xs) acc = loop xs (acc + 1)
