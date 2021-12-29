import Prelude hiding (Applicative ((<*>)))

-- The Monad instance of lists

-- instance Monad [] where
--   return x = [x]
--   xs >>= f = concat (map f xs)

list :: Num a => [a]
list = return 1

-- Bunny invasion

generation :: a -> [a]
generation = replicate 3

gen1, gen2, gen3, gen1', gen2', gen3' :: [String]
gen1 = ["bunny"]
gen2 = gen1 >>= generation
gen3 = gen2 >>= generation
gen1' = ["bunny", "rabbit"]
gen2' = gen1 >>= generation
gen3' = gen2 >>= generation

themselvesTimes :: [Int] -> [Int]
themselvesTimes ns = ns >>= (\n -> replicate n n)

-- Board game example

data Board

nextConfigs :: Board -> [Board]
nextConfigs board = undefined

threeTurns, threeTurns' :: Board -> [Board]
threeTurns board0 = do
  board1 <- nextConfigs board0
  board2 <- nextConfigs board1
  board3 <- nextConfigs board2
  return board3 -- return redundant, could just use `nextConfigs board2`

-- List comprehensions

threeTurns' bd0 = [bd3 | bd1 <- nextConfigs bd0, bd2 <- nextConfigs bd1, bd3 <- nextConfigs bd2]

(<*>) :: [a -> b] -> [a] -> [b]
fs <*> xs = [f x | f <- fs, x <- xs]
