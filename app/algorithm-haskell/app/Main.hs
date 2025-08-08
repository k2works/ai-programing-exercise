module Main (main) where

import FizzBuzz

main :: IO ()
main = mapM_ putStrLn $ take 20 generateList
