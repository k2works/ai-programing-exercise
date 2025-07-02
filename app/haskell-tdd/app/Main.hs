module Main where

import FizzBuzz

main :: IO ()
main = do
  putStrLn "FizzBuzz Application"
  putStrLn "==================="
  let result = generateList
  mapM_ putStrLn $ take 20 result
