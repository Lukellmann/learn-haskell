import Prelude hiding (Eq (..), Ord (..), Real (..))
import qualified Prelude as P (Eq ((==)), Ord)

-- Classes and instances

class Eq a where
  -- Minimal complete definition:
  -- (==) or (/=)
  (==), (/=) :: a -> a -> Bool
  x /= y = not (x == y)
  x == y = not (x /= y)

data Foo = Foo {x :: Integer, str :: String}

instance Eq Foo where
  (Foo x1 str1) == (Foo x2 str2) = (x1 P.== x2) && (str1 P.== str2)

-- Deriving

data Foo' = Foo' {x' :: Integer, str' :: String}
  deriving (P.Eq, P.Ord, Show)

-- Class inheritance

class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<), (<=), (>=), (>) :: a -> a -> Bool
  max, min :: a -> a -> a

class (Num a, Ord a) => Real a where
  -- | the rational equivalent of its real argument with full precision
  toRational :: a -> Rational

-- Type constraints

foo :: (Num a, Show a, Show b) => a -> a -> b -> String
foo x y t = show x ++ " + " ++ show y ++ show (x + y) ++ ". " ++ show t

-- A concerted example

class Located a where
  getLocation :: a -> (Int, Int)

class (Located a) => Movable a where
  setLocation :: (Int, Int) -> a -> a

data NamedPoint = NamedPoint
  { pointName :: String,
    pointX :: Int,
    pointY :: Int
  }
  deriving (Show)

instance Located NamedPoint where
  getLocation p = (pointX p, pointY p)

instance Movable NamedPoint where
  setLocation (x, y) p = p {pointX = x, pointY = y}

move :: Movable a => (Int, Int) -> a -> a
move (dx, dy) p = setLocation (x + dx, y + dy) p
  where
    (x, y) = getLocation p
