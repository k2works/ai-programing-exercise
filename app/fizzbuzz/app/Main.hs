module Main where

import FizzBuzz (FizzBuzzType(..), FizzBuzzValue(..), executeValueCommand, executeListCommand, getValues)

main :: IO ()
main = do
  putStrLn "=== FizzBuzz Command Pattern Demo ==="
  putStrLn "\n--- Value Command Examples ---"
  
  -- Value Command使用例
  putStrLn "Type1 Value Commands:"
  let valueResults = [executeValueCommand Type1 n | n <- [1, 3, 5, 15]]
  mapM_ printValueResult valueResults
  
  putStrLn "\n--- List Command Examples ---"
  
  -- List Command使用例
  putStrLn "Type1 List (1-15):"
  case executeListCommand Type1 15 of
    Right list -> do
      let values = getValues list
      mapM_ (\val -> putStrLn $ show (number val) ++ ": " ++ value val) values
    Left err -> putStrLn $ "Error: " ++ show err
    
  putStrLn "\nType2 List (1-10):"
  case executeListCommand Type2 10 of
    Right list -> do
      let values = getValues list
      mapM_ (\val -> putStrLn $ show (number val) ++ ": " ++ value val) values
    Left err -> putStrLn $ "Error: " ++ show err
    
  putStrLn "\nType3 List (12-18):"
  case executeListCommand Type3 8 of
    Right list -> do
      let values = getValues list
      mapM_ (\val -> putStrLn $ show (number val) ++ ": " ++ value val) values
    Left err -> putStrLn $ "Error: " ++ show err

printValueResult :: Either a FizzBuzzValue -> IO ()
printValueResult (Right val) = putStrLn $ "Success: " ++ show (number val) ++ " -> " ++ value val
printValueResult (Left _) = putStrLn "Error occurred"
