module Main where

import FizzBuzz

main :: IO ()
main = do
  putStrLn "=== FizzBuzz Command Pattern Demo ==="
  putStrLn "\n--- Generate Command Examples ---"
  
  -- Generate Command使用例
  putStrLn "Type1 Generate Commands:"
  let cmd1 = createGenerateCommand 1
      cmd3 = createGenerateCommand 3
      cmd5 = createGenerateCommand 5
      cmd15 = createGenerateCommand 15
  mapM_ (printCommandResult Type1) [cmd1, cmd3, cmd5, cmd15]
  
  putStrLn "\n--- List Output Command Examples ---"
  
  -- List Output Command使用例
  putStrLn "Type1 List (1-15):"
  let listCmd = createOutputCommand 15
  case runCommand listCmd Type1 of
    Right output -> putStr output
    Left err -> putStrLn $ "Error: " ++ show err
    
  putStrLn "\nType2 List (1-10):"
  let listCmd2 = createOutputCommand 10
  case runCommand listCmd2 Type2 of
    Right output -> putStr output
    Left err -> putStrLn $ "Error: " ++ show err
    
  putStrLn "\nType3 List (12-18):"
  let listCmd3 = createOutputCommand 8
  case runCommand listCmd3 Type3 of
    Right output -> putStr output
    Left err -> putStrLn $ "Error: " ++ show err

printCommandResult :: FizzBuzzType -> Command -> IO ()
printCommandResult fizzbuzzType cmd = 
  case runCommand cmd fizzbuzzType of
    Right result -> putStrLn $ "Success: " ++ result
    Left err -> putStrLn $ "Error: " ++ show err
