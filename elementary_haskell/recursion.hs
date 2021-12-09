import Numeric.Natural (Natural)
import Prelude hiding (length, replicate, zip, (!!), (++))

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

length :: [a] -> Natural
length [] = 0
length (_ : xs) = 1 + length xs

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

replicate :: Integral a => a -> b -> [b]
replicate n x
  | n < 1 = []
  | otherwise = x : replicate (n - 1) x

(!!) :: [a] -> Natural -> a
[] !! _ = error "index too large"
[x] !! 0 = x
(_ : xs) !! n = xs !! (n - 1)

zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x : xs) (y : ys) = (x, y) : zip xs ys

otherLength :: [a] -> Natural
otherLength xs = loop xs 0
  where
    loop [] acc = acc
    loop (_ : xs) acc = loop xs (acc + 1)
