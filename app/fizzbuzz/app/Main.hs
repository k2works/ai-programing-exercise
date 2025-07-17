module Main where

import           FizzBuzz (generate)

main :: IO ()
main = do
  putStrLn "FizzBuzz Numbers 1-30:"
  mapM_ (\n -> putStrLn $ show n ++ ": " ++ generate n) [1..30]
