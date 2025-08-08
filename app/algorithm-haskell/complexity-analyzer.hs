#!/usr/bin/env stack
{- stack script --resolver lts-24.3 
   --package base,directory,filepath,text,containers
-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory
import System.FilePath
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (forM_, when)
import Data.Maybe (fromMaybe)

-- 循環複雑度を計算する関数
calculateComplexity :: T.Text -> Int
calculateComplexity content = 
    let lines' = T.lines content
        complexity = foldl countComplexityInLine 1 lines'
    in complexity

-- 各行で複雑度を増加させる要因をカウント
countComplexityInLine :: Int -> T.Text -> Int
countComplexityInLine acc line =
    let trimmed = T.strip line
        -- コメントや文字列を除外
        cleaned = removeComments trimmed
        complexityFactors = [
            "if ",
            "case ",
            "guard",
            "|",           -- ガード
            "&&",          -- 論理積
            "||",          -- 論理和
            ">>",          -- モナドのバインド
            "do",          -- do記法
            "where",       -- where句
            "let "         -- let式
            ]
        count = length $ filter (`T.isInfixOf` cleaned) complexityFactors
    in acc + count

-- コメントを除去する簡単な実装
removeComments :: T.Text -> T.Text
removeComments line
    | "--" `T.isInfixOf` line = T.takeWhile (/= '-') line
    | otherwise = line

-- ファイルの複雑度を分析
analyzeFile :: FilePath -> IO ()
analyzeFile filePath = do
    content <- TIO.readFile filePath
    let complexity = calculateComplexity content
        lineCount = length $ T.lines content
        avgComplexity = fromIntegral complexity / fromIntegral lineCount :: Double
        roundedAvg = fromIntegral (round (avgComplexity * 100)) / 100 :: Double
    
    putStrLn $ "ファイル: " ++ filePath
    putStrLn $ "  循環複雑度: " ++ show complexity
    putStrLn $ "  行数: " ++ show lineCount
    putStrLn $ "  平均複雑度: " ++ show roundedAvg
    
    when (complexity > 10) $
        putStrLn $ "  ⚠️  警告: 循環複雑度が高すぎます (推奨: 10以下)"
    when (complexity > 20) $
        putStrLn $ "  🚨 重要: 循環複雑度が非常に高いです。リファクタリングを検討してください"
    putStrLn ""

-- Haskellファイルを再帰的に探索
findHaskellFiles :: FilePath -> IO [FilePath]
findHaskellFiles dir = do
    exists <- doesDirectoryExist dir
    if exists
        then do
            contents <- listDirectory dir
            files <- mapM (\f -> do
                let fullPath = dir </> f
                isDir <- doesDirectoryExist fullPath
                if isDir && not ("." `isPrefixOf` f) && f /= ".stack-work"
                    then findHaskellFiles fullPath
                    else if takeExtension f == ".hs"
                        then return [fullPath]
                        else return []
                ) contents
            return $ concat files
        else return []

main :: IO ()
main = do
    putStrLn "🔍 Haskellコードの循環複雑度分析"
    putStrLn "================================="
    
    srcFiles <- findHaskellFiles "src"
    testFiles <- findHaskellFiles "test"
    appFiles <- findHaskellFiles "app"
    
    let allFiles = srcFiles ++ testFiles ++ appFiles
    
    if null allFiles
        then putStrLn "❌ Haskellファイルが見つかりませんでした"
        else do
            putStrLn $ "📁 " ++ show (length allFiles) ++ " 個のHaskellファイルを分析中...\n"
            forM_ allFiles analyzeFile
            
            -- 総合統計
            complexities <- mapM (\f -> do
                content <- TIO.readFile f
                return $ calculateComplexity content
                ) allFiles
            
            let totalComplexity = sum complexities
                avgComplexity = fromIntegral totalComplexity / fromIntegral (length allFiles) :: Double
                maxComplexity = maximum complexities
                roundedAvg = fromIntegral (round (avgComplexity * 100)) / 100 :: Double
                
            putStrLn "📊 総合統計:"
            putStrLn $ "  総循環複雑度: " ++ show totalComplexity
            putStrLn $ "  平均循環複雑度: " ++ show roundedAvg
            putStrLn $ "  最大循環複雑度: " ++ show maxComplexity
            
            if avgComplexity > 10
                then putStrLn "⚠️  プロジェクト全体の複雑度が高めです。リファクタリングを検討してください。"
                else putStrLn "✅ プロジェクトの複雑度は適切な範囲内です。"
