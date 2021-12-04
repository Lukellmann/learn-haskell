{-
compile with:
ghc --make -o bonjourWorld standalone_programs.hs
-}

module Main where

import Hello (hello)

main :: IO ()
main = putStrLn hello
