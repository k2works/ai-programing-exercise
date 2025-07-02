module Main (main) where

import FizzBuzz

main :: IO ()
main = do
  putStrLn "FizzBuzz Game - 1から100まで"
  mapM_ (putStrLn . generate) [1..100]
