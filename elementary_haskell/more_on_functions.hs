import Data.List (delete)
import Prelude hiding (elem)

-- let and where revisited

addStr :: (Num a, Read a) => a -> String -> a
addStr x str = x + read str

sumStr, sumStrLet, sumStrWhere :: (Num a, Read a) => [String] -> a
sumStr = foldl addStr 0
sumStrLet =
  let addStr x str = x + read str
   in foldl addStr 0
sumStrWhere = foldl addStr 0
  where
    addStr x str = x + read str

f x =
  if x > 0
    then (let lsq = log x ^ 2 in tan lsq) * sin x
    else 0

data Color = Black | White | RGB Int Int Int

describeColour :: Color -> String
describeColour c =
  "This colour "
    ++ case c of
      Black -> "is black"
      White -> "is white"
      RGB red green blue -> " has an average of the components of " ++ show av
        where
          av = (red + green + blue) `div` 3
    ++ ", yeah?"

doStuff :: (Num a, Ord a) => a -> String
doStuff x
  | x < 3 = report "less than three"
  | otherwise = report "normal"
  where
    report y = "the input is " ++ y

-- Anonymous Functions - lambdas

sumStrLambda :: (Num a, Read a) => [String] -> a
sumStrLambda = foldl (\x str -> x + read str) 0

tail' = \(_ : xs) -> xs

-- Operators

(\\) :: Eq a => [a] -> [a] -> [a]
xs \\ ys = foldl (\zs y -> delete y zs) xs ys

multiplyList :: Num a => a -> [a] -> [a]
multiplyList m = map (m *)

elem :: Eq a => a -> [a] -> Bool
x `elem` xs = any (== x) xs

g :: Num a => [a] -> [a]
g = map (\x -> x * 2 + 3)

h :: (Num a, Read a) => [String] -> a
h = foldr (\x y -> read x + y) 1

i :: Num a => a -> a
i = \x -> 4 + x

j :: (Num a, Eq a) => [a] -> Bool
j = \xs -> 1 `elem` xs

k :: Char -> Bool
k = \c -> c `notElem` "abc"
