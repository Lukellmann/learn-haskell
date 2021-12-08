-- Trees

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)

data List a = Nil | Cons a (List a) deriving (Show)

listMap :: (a -> b) -> List a -> List b
listMap _ Nil = Nil
listMap f (Cons x xs) = Cons (f x) (listMap f xs)

treeMap, treeMap' :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf x) = Leaf (f x)
treeMap f (Branch left right) = Branch (treeMap f left) (treeMap f right)
treeMap' f = g
  where
    g (Leaf x) = Leaf (f x)
    g (Branch left right) = Branch (g left) (g right)

listFoldr :: (a -> b -> b) -> b -> List a -> b
listFoldr _ acc Nil = acc
listFoldr f acc (Cons x xs) = f x (listFoldr f acc xs)

treeFold :: (b -> b -> b) -> (a -> b) -> Tree a -> b
treeFold fBranch fLeaf = g
  where
    g (Leaf x) = fLeaf x
    g (Branch left right) = fBranch (g left) (g right)

tree1 :: Num a => Tree a
tree1 =
  Branch
    ( Branch
        ( Branch
            (Leaf 1)
            (Branch (Leaf 2) (Leaf 3))
        )
        ( Branch
            (Leaf 4)
            (Branch (Leaf 5) (Leaf 6))
        )
    )
    ( Branch
        (Branch (Leaf 7) (Leaf 8))
        (Leaf 9)
    )

doubleTree :: Num a => Tree a -> Tree a
doubleTree = treeMap (* 2)

sumTree :: Num a => Tree a -> a
sumTree = treeFold (+) id

fringeTree :: Tree a -> [a]
fringeTree = treeFold (++) (: [])

-- Other datatypes

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing = Nothing
maybeMap f (Just x) = Just (f x)

data Weird a b
  = First a
  | Second b
  | Third [(a, b)]
  | Fourth (Weird a b)
  | Fifth [Weird a b] a (Weird a a, Maybe (Weird a b))

weirdMap :: (a -> c) -> (b -> d) -> Weird a b -> Weird c d
weirdMap fa fb = g
  where
    g (First a) = First (fa a)
    g (Second b) = Second (fb b)
    g (Third list) = Third [(fa a, fb b) | (a, b) <- list] -- or: g (Third list) = Third (map (\(a, b) -> (fa a, fb b)) list)
    g (Fourth weird) = Fourth (g weird)
    g (Fifth list a (weirdaa, maybe)) = Fifth (map g list) (fa a) (weirdMap fa fa weirdaa, maybeMap g maybe)

weirdFold :: (a -> c) -> (b -> c) -> ([(a, b)] -> c) -> (c -> c) -> ([c] -> a -> (Weird a a, Maybe c) -> c) -> Weird a b -> c
weirdFold f1 f2 f3 f4 f5 = g
  where
    g (First a) = f1 a
    g (Second b) = f2 b
    g (Third list) = f3 list
    g (Fourth weird) = f4 (g weird)
    g (Fifth list a (weirdaa, maybe)) = f5 (map g list) a (weirdaa, maybeMap g maybe)

listFoldr' :: (a -> b -> b) -> b -> List a -> b
listFoldr' fCons fNil = g
  where
    g (Cons x xs) = fCons x (g xs)
    g Nil = fNil
