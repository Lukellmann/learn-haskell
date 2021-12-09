import Prelude hiding (signum)

signum x
  | x < 0 = -1
  | x > 0 = 1
  | otherwise = 0

pts :: Int -> Int
pts 1 = 10
pts 2 = 6
pts x
  | x < 1 || x > 7 = 0
  | otherwise = 7 - x

fst' :: (a, b) -> a
fst' (x, _) = x

snd' :: (a, b) -> b
snd' (_, x) = x

head' :: [a] -> a
head' (x : _) = x
head' [] = error "head: empty list"

tail' :: [a] -> [a]
tail' (_ : xs) = xs
tail' [] = error "tail: empty list"

roots :: Floating b => b -> b -> b -> (b, b)
roots a b c =
  let sdisc = sqrt (b * b - 4 * a * c)
      twiceA = 2 * a
   in ((- b + sdisc) / twiceA, (- b - sdisc) / twiceA)
