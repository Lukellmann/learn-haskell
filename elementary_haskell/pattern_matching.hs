-- Analysing pattern matching

mappm _ [] = []
mappm f (x : xs) = f x : mappm f xs

data Foo = Bar | Baz Int

f :: Foo -> Int
f Bar = 1
f (Baz x) = x - 1

dropThree :: [a] -> [a]
dropThree (_ : _ : _ : xs) = xs
dropThree _ = []

fstPlusSnd :: Num a => (a, a) -> a
fstPlusSnd (x, y) = x + y

norm3D :: Floating a => (a, a, a) -> a
norm3D (x, y, z) = sqrt (x ^ 2 + y ^ 2 + z ^ 2)

-- Matching literal values

g :: (Num a, Eq a, Num b) => a -> b
g 0 = 1
g 1 = 5
g 2 = 2
g _ = -1

h :: (Num a, Eq a) => [a] -> Bool
h [0] = False
h (0 : _) = True
h _ = False

-- Syntax tricks

contrievedMap :: ([a] -> a -> b) -> [a] -> [b]
contrievedMap _ [] = []
contrievedMap f list@(x : xs) = f list x : contrievedMap f xs

scanrAsPattern :: (a -> b -> b) -> b -> [a] -> [b]
scanrAsPattern _ initial [] = [initial]
scanrAsPattern step initial (x : xs) =
  let prev@(hprev : _) = scanrAsPattern step initial xs
   in step x hprev : prev

data Foo2 = Bar2 | Baz2 {bazNumber :: Int, bazName :: String}

i :: Foo2 -> Int
i Baz2 {bazName = name} = length name
i Bar2 {} = 0

x = Baz2 1 "Haskell"

y = Baz2 {bazName = "Curry", bazNumber = 2}

j :: Foo -> Bool
j Bar {} = True
j Baz {} = False

-- Where we can use pattern matching

z = let (x : _) = map (* 2) [1, 2, 3] in x + 5

z' = x + 5 where (x : _) = map (* 2) [1, 2, 3]

swap = \(x, y) -> (y, x)

data MyMaybe a = MyNothing | MyJust a

catMyMaybes :: [MyMaybe a] -> [a]
catMyMaybes ms = [x | MyJust x <- ms]

putFirstChar :: IO ()
putFirstChar = do
  (c : _) <- getLine
  putStrLn [c]
