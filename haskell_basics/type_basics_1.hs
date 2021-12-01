xor, f :: Bool -> Bool -> Bool
xor p q = (p || q) && not (p && q)
f x y   = not x && y

g :: Int -> Int
g x = (2 * x - 1) ^ 2

isL :: Char -> Bool
isL c = c == 'l'
