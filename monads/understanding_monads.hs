import Control.Monad (ap, liftM)
import Prelude hiding (Monad (..))
import qualified Prelude as P (Monad (return, (>>), (>>=)))

-- Definition

class Applicative m => Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b

  return = pure
  m >> n = m >>= \_ -> n

instance Monad Maybe where
  return = Just
  m >>= f = case m of
    Nothing -> Nothing
    Just x -> f x

data Person -- ...

father, mother, maternalGrandfather, maternalGrandfather' :: Person -> Maybe Person
father = undefined
mother = undefined
maternalGrandfather p = case mother p of
  Nothing -> Nothing
  Just mom -> father mom

bothGrandfathers, bothGrandfathers', bothGrandfathers'', bothGrandfathers''' :: Person -> Maybe (Person, Person)
bothGrandfathers p = case father p of
  Nothing -> Nothing
  Just dad -> case father dad of
    Nothing -> Nothing
    Just gf1 -> case mother p of
      Nothing -> Nothing
      Just mom -> case father mom of
        Nothing -> Nothing
        Just gf2 -> Just (gf1, gf2)

maternalGrandfather' p = mother p >>= father

bothGrandfathers' p =
  father p >>=
    (\dad -> father dad >>=
      (\gf1 -> mother p >>=
        (\mom -> father mom >>=
          (\gf2 -> return (gf1, gf2) ))))

instance Monad IO where
  return = P.return
  (>>=) = (P.>>=)
  (>>) = (P.>>)

printSomethingTwice :: String -> IO ()
printSomethingTwice str = putStrLn str >> putStrLn str

-- Notions of Computation

bothGrandfathers'' p = do {
  dad <- father p;
  gf1 <- father dad;
  mom <- mother p;
  gf2 <- father mom;
  return (gf1, gf2);
}

-- Monad Laws

bothGrandfathers''' p =
  (father p >>= father) >>=
    (\gf1 -> (mother p >>= father) >>=
      (\gf2 -> return (gf1,gf2) ))

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f >=> g = \x -> f x >>= g

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
g <=< f = f >=> g

-- liftM and Friends

data Foo a

instance P.Monad Foo where
  return = undefined -- ...
  (>>=) = undefined -- ...

instance Applicative Foo where
  pure = P.return
  (<*>) = ap

instance Functor Foo where
  fmap = liftM
