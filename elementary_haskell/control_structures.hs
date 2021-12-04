-- if and guards revisited

describeLetterIf :: Char -> String
describeLetterIf c =
  if c >= 'a' && c <= 'z'
    then "Lower case"
    else
      if c >= 'A' && c <= 'Z'
        then "Upper case"
        else "Not an ASCII letter"

describeLetterGuard :: Char -> String
describeLetterGuard c
  | c >= 'a' && c <= 'z' = "Lower case"
  | c >= 'A' && c <= 'Z' = "Upper case"
  | otherwise = "Not an ASCII letter"

g x y = (if x == 0 then 1 else sin x / x) * y

-- case expressions

f 0 = 18
f 1 = 15
f 2 = 12
f x = 12 - x

f' x = case x of
  0 -> 18
  1 -> 15
  2 -> 12
  _ -> 12 - x

describeString :: String -> String
describeString str = case str of
  (x : xs) ->
    "The first character of the string is '"
      ++ [x]
      ++ "' and there are "
      ++ show (length xs)
      ++ " more characters in it."
  [] -> "This is an empty string."

data Color = Black | White | RGB Int Int Int

describeBlackOrWhite :: Color -> String
describeBlackOrWhite c =
  "This color is"
    ++ case c of
      Black -> " black"
      White -> " white"
      RGB 0 0 0 -> " black"
      RGB 255 255 255 -> " white"
      _ -> "... uh... something else"
    ++ ", yeah?"

fakeIf :: Bool -> a -> a -> a
fakeIf cond ifTrue ifFalse = case cond of
  True -> ifTrue
  False -> ifFalse

-- Controlling actions, revisited

doGuessing :: (Ord a, Read a) => a -> IO ()
doGuessing num = do
  putStrLn "Enter your guess:"
  guess <- getLine
  case compare (read guess) num of
    LT -> do
      putStrLn "Too low!"
      doGuessing num
    GT -> do
      putStrLn "Too high!"
      doGuessing num
    EQ -> putStrLn "You win!"

greetSpecificCase :: IO ()
greetSpecificCase = do
  putStrLn "Please enter your name:"
  name <- getLine
  putStrLn
    ( case name of
        "Simon" -> greatLanguage
        "John" -> greatLanguage
        "Phil" -> greatLanguage
        "Koen" -> "I think Debugging Haskell is fun!"
        _ -> "Sorry, I dont't know who you are :/"
    )
  where
    greatLanguage = "I think Haskell is a great programming language!"
