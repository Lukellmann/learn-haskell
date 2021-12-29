import Control.Monad ((<=<), (>=>))
import Data.Maybe (fromMaybe)

-- Safe functions

safeLog, safeSqrt, safeLogSqrt :: (Floating a, Ord a) => a -> Maybe a
safeLog x
  | x > 0 = Just (log x)
  | otherwise = Nothing
safeSqrt x
  | x >= 0 = Just (sqrt x)
  | otherwise = Nothing
safeLogSqrt = safeLog <=< safeSqrt

unsafeLogSqrt :: Floating a => a -> a
unsafeLogSqrt = log . sqrt

-- Lookup tables

phonebook :: [(String, String)]
phonebook =
  [ ("Bob", "01788 665242"),
    ("Fred", "01624 556442"),
    ("Alice", "01889 985333"),
    ("Jane", "01732 187565")
  ]

governmentDatabase :: [(String, String)]
governmentDatabase = undefined

taxDatabase :: [(String, Double)]
taxDatabase = undefined

getRegistrationNumber :: String -> Maybe String
getRegistrationNumber name = lookup name phonebook >>= (`lookup` governmentDatabase)

getTaxOwed, getTaxOwed', getTaxOwed'', getTaxOwed''' :: String -> Maybe Double
getTaxOwed name =
  lookup name phonebook
    >>= (`lookup` governmentDatabase)
    >>= (`lookup` taxDatabase)
getTaxOwed' name = do
  number <- lookup name phonebook
  registration <- lookup number governmentDatabase
  lookup registration taxDatabase
getTaxOwed'' = (`lookup` taxDatabase) <=< (`lookup` governmentDatabase) <=< (`lookup` phonebook)
getTaxOwed''' = (`lookup` phonebook) >=> (`lookup` governmentDatabase) >=> (`lookup` taxDatabase)

-- Extracting values

zeroAsDefault, zeroAsDefault' :: Num a => Maybe a -> a
zeroAsDefault mx = case mx of
  Nothing -> 0
  Just x -> x
zeroAsDefault' = fromMaybe 0

displayResult :: Show a => Maybe a -> String
displayResult = maybe "There was no result" (("The result was " ++) . show)
