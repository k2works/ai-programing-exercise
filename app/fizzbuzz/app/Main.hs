module Main where

import FizzBuzz (FizzBuzzType(..), generate)

main :: IO ()
main = do
  putStrLn "Type1 FizzBuzz Numbers 1-30:"
  mapM_ (\n -> putStrLn $ show n ++ ": " ++ generate Type1 n) [1..30]
  putStrLn "\nType2 Numbers 1-15:"
  mapM_ (\n -> putStrLn $ show n ++ ": " ++ generate Type2 n) [1..15]
  putStrLn "\nType3 FizzBuzz Cases 1-15:"
  mapM_ (\n -> putStrLn $ show n ++ ": " ++ generate Type3 n) [1..15]
