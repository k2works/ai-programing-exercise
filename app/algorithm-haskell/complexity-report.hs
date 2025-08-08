#!/usr/bin/env stack
{- stack script --resolver lts-24.3 
   --package base,directory,filepath,text,containers,time
-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory
import System.FilePath
import Data.List (isInfixOf, isPrefixOf, sortBy)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (forM_, when)
import Data.Maybe (fromMaybe)
import Data.Time
import Data.Ord (comparing)

-- 複雑度情報を格納するデータ型
data ComplexityInfo = ComplexityInfo
    { fileName :: FilePath
    , complexity :: Int
    , lineCount :: Int
    , avgComplexity :: Double
    , riskLevel :: RiskLevel
    } deriving (Show)

data RiskLevel = Low | Medium | High | VeryHigh deriving (Show, Eq, Ord)

-- リスクレベルの判定
assessRisk :: Int -> RiskLevel
assessRisk comp
    | comp <= 5    = Low
    | comp <= 10   = Medium
    | comp <= 20   = High
    | otherwise    = VeryHigh

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
analyzeFile :: FilePath -> IO ComplexityInfo
analyzeFile filePath = do
    content <- TIO.readFile filePath
    let comp = calculateComplexity content
        lineCount' = length $ T.lines content
        avgComp = fromIntegral comp / fromIntegral lineCount' :: Double
        risk = assessRisk comp
    
    return $ ComplexityInfo filePath comp lineCount' avgComp risk

-- 詳細なレポートを生成
generateDetailedReport :: [ComplexityInfo] -> IO ()
generateDetailedReport infos = do
    putStrLn "🔍 Haskellコードの循環複雑度分析"
    putStrLn "================================="
    
    -- ファイル別詳細
    putStrLn "\n📄 ファイル別詳細:"
    let sortedInfos = sortBy (comparing (negate . complexity)) infos
    forM_ sortedInfos $ \info -> do
        let risk = riskLevel info
            riskEmoji = case risk of
                Low -> "✅"
                Medium -> "⚠️ "
                High -> "🔶"
                VeryHigh -> "🚨"
        
        putStrLn $ riskEmoji ++ " " ++ fileName info
        putStrLn $ "    循環複雑度: " ++ show (complexity info) ++ " (" ++ show risk ++ ")"
        putStrLn $ "    行数: " ++ show (lineCount info)
        putStrLn $ "    平均複雑度: " ++ show (fromIntegral (round (avgComplexity info * 100)) / 100 :: Double)
        
        when (complexity info > 10) $
            putStrLn "    💡 推奨: リファクタリングを検討してください"
        putStrLn ""
    
    -- 統計情報
    let totalComplexity = sum $ map complexity infos
        avgComplexity' = fromIntegral totalComplexity / fromIntegral (length infos) :: Double
        maxComplexity = maximum $ map complexity infos
        lowCount = length $ filter ((== Low) . riskLevel) infos
        mediumCount = length $ filter ((== Medium) . riskLevel) infos
        highCount = length $ filter ((== High) . riskLevel) infos
        veryHighCount = length $ filter ((== VeryHigh) . riskLevel) infos
        
    putStrLn "📊 総合統計:"
    putStrLn $ "  総循環複雑度: " ++ show totalComplexity
    putStrLn $ "  平均循環複雑度: " ++ show (fromIntegral (round (avgComplexity' * 100)) / 100 :: Double)
    putStrLn $ "  最大循環複雑度: " ++ show maxComplexity
    putStrLn $ "  ファイル数: " ++ show (length infos)
    putStrLn ""
    
    putStrLn "🎯 リスク分布:"
    putStrLn $ "  ✅ 低リスク (1-5): " ++ show lowCount ++ " ファイル"
    putStrLn $ "  ⚠️  中リスク (6-10): " ++ show mediumCount ++ " ファイル"
    putStrLn $ "  🔶 高リスク (11-20): " ++ show highCount ++ " ファイル"
    putStrLn $ "  🚨 非常に高リスク (21+): " ++ show veryHighCount ++ " ファイル"
    putStrLn ""
    
    -- 推奨事項
    let highRiskFiles = filter ((\r -> r == High || r == VeryHigh) . riskLevel) infos
    if null highRiskFiles
        then putStrLn "✅ 全てのファイルが適切な複雑度範囲内です。"
        else do
            putStrLn "⚠️  高リスクファイルが見つかりました:"
            forM_ highRiskFiles $ \info -> 
                putStrLn $ "   - " ++ fileName info ++ " (複雑度: " ++ show (complexity info) ++ ")"
            putStrLn ""
            putStrLn "💡 推奨事項:"
            putStrLn "   - 複雑な関数を小さな関数に分割する"
            putStrLn "   - 深いネストを避ける"
            putStrLn "   - パターンマッチングを活用する"
            putStrLn "   - ガード条件を簡素化する"
    
    -- レポートタイムスタンプ
    currentTime <- getCurrentTime
    putStrLn $ "\n📅 レポート生成日時: " ++ show currentTime

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
    srcFiles <- findHaskellFiles "src"
    testFiles <- findHaskellFiles "test"
    appFiles <- findHaskellFiles "app"
    
    let allFiles = srcFiles ++ testFiles ++ appFiles
    
    if null allFiles
        then putStrLn "❌ Haskellファイルが見つかりませんでした"
        else do
            infos <- mapM analyzeFile allFiles
            generateDetailedReport infos
