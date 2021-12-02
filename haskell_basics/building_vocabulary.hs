import Data.List

-- Function composition

f :: Num a => a -> a
f x = x + 3

square :: Num a => a -> a
square x = x * x

squareOfF :: Num a => a -> a
squareOfF = square . f

fOfSquare :: Num a => a -> a
fOfSquare = f . square

-- Prelude and the libraries

preludePermutations :: [String]
preludePermutations = permutations "Prelude"

-- One exhibit

revWords :: String -> String
revWords = unwords . reverse . words
