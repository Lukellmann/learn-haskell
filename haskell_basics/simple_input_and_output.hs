import Language.Haskell.TH (doublePrimL)

-- Sequencing actions with do

greet :: IO ()
greet = do
  putStrLn "Please enter your name:"
  name <- getLine
  putStrLn ("Hello, " ++ name ++ ", how are you?")

triangle :: IO ()
triangle = do
  putStrLn "The base?"
  base <- getLine
  putStrLn "The height?"
  height <- getLine
  putStrLn ("The area of that triangle is " ++ show (read base * read height / 2))

greetGeneric :: IO ()
greetGeneric = do
  putStrLn "Please enter your name:"
  getLine
  putStrLn "Hello, how are you?"

uselessPut :: IO ()
uselessPut = do
  x <- putStrLn "Hi"
  putStrLn "Hi"

acceptOrRepeatGuessing :: (Ord a, Read a) => a -> a -> IO ()
acceptOrRepeatGuessing num guess
  | guess < num = do
    putStrLn "Too low!"
    doGuessing num
  | guess > num = do
    putStrLn "Too high!"
    doGuessing num
  | otherwise = do putStrLn "You win!"

doGuessing :: (Ord a, Read a) => a -> IO ()
doGuessing num = do
  putStrLn "Enter your guess:"
  guess <- getLine
  acceptOrRepeatGuessing num (read guess)

greetSpecificIfThen :: IO ()
greetSpecificIfThen = do
  putStrLn "Please enter your name:"
  name <- getLine
  putStrLn
    ( if name == "Simon" || name == "John" || name == "Phil"
        then greatLanguage
        else if name == "Koen" then debugging else unknown
    )
  where
    greatLanguage = "I think Haskell is a great programming language!"
    debugging = "I think Debugging Haskell is fun!"
    unknown = "Sorry, I dont't know who you are :/"

greetSpecificWhere :: IO ()
greetSpecificWhere = do
  putStrLn "Please enter your name:"
  name <- getLine
  putStrLn (greeting name)
  where
    greatLanguage = "I think Haskell is a great programming language!"
    greeting "Simon" = greatLanguage
    greeting "John" = greatLanguage
    greeting "Phil" = greatLanguage
    greeting "Koen" = "I think Debugging Haskell is fun!"
    greeting _ = "Sorry, I dont't know who you are :/"
