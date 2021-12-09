import Text.Read (readMaybe)
import Prelude hiding (Applicative (..), (<$>))
import qualified Prelude as P (Applicative (pure, (<*>)))

-- Scene 1: Applicative

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

interactiveDoubling :: IO ()
interactiveDoubling = do
  putStrLn "Choose a number:"
  input <- getLine
  case (2 *) <$> readMaybe input :: Maybe Double of
    Just x -> putStrLn ("The double of your number is " ++ show x)
    Nothing -> do
      putStrLn "This is not a valid number. Retrying..."
      interactiveDoubling

interactiveSumming, interactiveSumming' :: IO ()
interactiveSumming = do
  putStrLn "Choose two numbers:"
  sx <- getLine
  sy <- getLine
  let mx = readMaybe sx :: Maybe Double
      my = readMaybe sy
  case mx of
    Just x -> case my of
      Just y -> putStrLn ("The sum of your numbers is " ++ show (x + y))
      Nothing -> retry
    Nothing -> retry
  where
    retry = do
      putStrLn "Invalid number. Retrying..."
      interactiveSumming
interactiveSumming' = do
  first <- chooseNumber "first"
  second <- chooseNumber "second"
  putStrLn ("The sum of your numbers is " ++ show (first + second))
  where
    chooseNumber num = do
      putStrLn ("Choose " ++ num ++ " number:")
      input <- getLine
      case readMaybe input :: Maybe Double of
        Just x -> return x
        Nothing -> do
          putStrLn "This is not a valid number. Retrying..."
          chooseNumber num

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

  (*>) :: f a -> f b -> f b
  u *> v = fmap (\_ y -> y) u <*> v

  (<*) :: f a -> f b -> f a
  u <* v = fmap const u <*> v

instance Applicative Maybe where
  pure = Just
  (Just f) <*> (Just x) = Just (f x)
  _ <*> _ = Nothing

interactiveSumming'' :: IO ()
interactiveSumming'' = do
  putStrLn "Choose two numbers:"
  sx <- getLine
  sy <- getLine
  let mx = readMaybe sx :: Maybe Double
      my = readMaybe sy
  case (+) <$> mx <*> my of
    Just z -> putStrLn ("The sum of your numbers is " ++ show z)
    Nothing -> do
      putStrLn "Invalid number. Retrying..."
      interactiveSumming''

-- Scene 2: IO

interactiveSumming''' :: IO ()
interactiveSumming''' = do
  putStrLn "Choose two numbers:"
  mx <- readMaybe <$> getLine -- <-- equivalent
  my <- fmap readMaybe getLine -- <-/
  case (+) <$> mx <*> my :: Maybe Double of
    Just z -> putStrLn ("The sum of your numbers is " ++ show z)
    Nothing -> do
      putStrLn "Invalid number. Retrying..."
      interactiveSumming'''

interactiveConcatenating :: IO ()
interactiveConcatenating = do
  putStrLn "Choose two strings:"
  sx <- getLine
  sy <- getLine
  putStrLn "Let's concatenate them:"
  putStrLn (sx ++ sy)

instance Applicative IO where
  pure = P.pure
  (<*>) = (P.<*>)

interactiveConcatenating', interactiveConcatenating'', interactiveConcatenating''' :: IO ()
interactiveConcatenating' = do
  putStrLn "Choose two strings:"
  s <- (++) <$> getLine <*> getLine
  putStrLn "Let's concatenate them:"
  putStrLn s
interactiveConcatenating'' = do
  putStrLn "Choose two strings:"
  s <- (++) <$> getLine <*> getLine
  putStrLn "Let's concatenate them:" *> putStrLn s
interactiveConcatenating''' = do
  s <- putStrLn "Choose two strings:" *> ((++) <$> getLine <*> getLine)
  putStrLn "Let's concatenate them:" *> putStrLn s
