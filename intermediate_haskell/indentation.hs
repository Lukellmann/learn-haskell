foo, foo', foo'' :: Double -> Double

foo x =
  let s = sin x
      c = cos x
   in 2 * s * c

foo' x = let {
  s = sin x;
  c = cos x;
} in 2 * s * c

foo'' x = let { s = sin x; c = cos x } in 2 * s * c

doGuessing :: (Ord a, Read a) => a -> IO ()
doGuessing num = do {
  putStrLn "Enter your guess:";
  guess <- getLine;
  case compare (read guess) num of {
    LT -> do {
      putStrLn "Too low!";
      doGuessing num;
    };
    GT -> do {
      putStrLn "Too high!";
      doGuessing num;
    };
    EQ -> putStrLn "You win!";
  };
};
