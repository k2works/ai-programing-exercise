{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import System.Timeout

-- リソースの種類（コンストラクタは英語）
data Resource = Controller | Remote deriving (Show, Eq)

-- リソースの状態（誰が持っているか）
type ResourceState = MVar (Maybe String)

-- リソースマネージャー
data ResourceManager = ResourceManager
    { controllerState :: ResourceState
    , remoteState     :: ResourceState
    }

-- リソースマネージャーを作成
createManager :: IO ResourceManager
createManager = do
    ctrl <- newMVar Nothing
    rmt  <- newMVar Nothing
    return $ ResourceManager ctrl rmt

-- リソースを取得（タイムアウト付き）
tryAcquire :: ResourceManager -> Resource -> String -> Int -> IO Bool
tryAcquire manager resource taskName timeoutMs = do
    let state = case resource of
            Controller -> controllerState manager
            Remote     -> remoteState manager

    result <- timeout (timeoutMs * 1000) $ do
        takeMVar state
        putMVar state (Just taskName)
        putStrLn $ "  " ++ taskName ++ ": " ++ show resource ++ " を取得！"
        return True

    return $ maybe False id result

-- リソースを解放
release :: ResourceManager -> Resource -> String -> IO ()
release manager resource taskName = do
    let state = case resource of
            Controller -> controllerState manager
            Remote     -> remoteState manager

    takeMVar state
    putMVar state Nothing
    putStrLn $ "  " ++ taskName ++ ": " ++ show resource ++ " を解放"

-- デッドロックが起きるパターン
deadlockScenario :: IO ()
deadlockScenario = do
    putStrLn "\n=== デッドロックが起きるパターン（逆順）==="
    putStrLn "たろう: Controller → Remote"
    putStrLn "はなこ: Remote → Controller\n"

    manager <- createManager
    let timeoutMs = 2000

    taroResult   <- newEmptyMVar
    hanakoResult <- newEmptyMVar

    -- たろう: Controller → Remote の順
    _ <- forkIO $ do
        putStrLn "たろう: 開始"
        gotController <- tryAcquire manager Controller "たろう" timeoutMs
        when gotController $ do
            threadDelay 100000  -- 100ms待つ
            gotRemote <- tryAcquire manager Remote "たろう" timeoutMs
            if gotRemote
                then do
                    putStrLn "たろう: ゲーム完了！"
                    release manager Remote "たろう"
                    release manager Controller "たろう"
                    putMVar taroResult "完了"
                else do
                    putStrLn "たろう: Remoteが取れない...タイムアウト！"
                    release manager Controller "たろう"
                    putMVar taroResult "デッドロック"

    -- はなこ: Remote → Controller の順（逆！）
    _ <- forkIO $ do
        putStrLn "はなこ: 開始"
        gotRemote <- tryAcquire manager Remote "はなこ" timeoutMs
        when gotRemote $ do
            threadDelay 100000
            gotController <- tryAcquire manager Controller "はなこ" timeoutMs
            if gotController
                then do
                    putStrLn "はなこ: ゲーム完了！"
                    release manager Controller "はなこ"
                    release manager Remote "はなこ"
                    putMVar hanakoResult "完了"
                else do
                    putStrLn "はなこ: Controllerが取れない...タイムアウト！"
                    release manager Remote "はなこ"
                    putMVar hanakoResult "デッドロック"

    -- 結果を待つ
    taro   <- takeMVar taroResult
    hanako <- takeMVar hanakoResult
    putStrLn $ "\n結果: たろう=" ++ taro ++ ", はなこ=" ++ hanako

-- デッドロックを防ぐパターン
safeScenario :: IO ()
safeScenario = do
    putStrLn "\n=== デッドロックを防ぐパターン（同順）==="
    putStrLn "両方: Controller → Remote（同じ順番）\n"

    manager <- createManager
    let timeoutMs = 5000

    taroResult   <- newEmptyMVar
    hanakoResult <- newEmptyMVar

    -- 同じ順番で取得する関数
    let safeTask name resultVar = do
            putStrLn $ name ++ ": 開始"
            gotController <- tryAcquire manager Controller name timeoutMs
            when gotController $ do
                threadDelay 100000
                gotRemote <- tryAcquire manager Remote name timeoutMs
                when gotRemote $ do
                    putStrLn $ name ++ ": ゲーム完了！"
                    threadDelay 200000  -- ゲームプレイ
                    release manager Remote name
                    release manager Controller name
                    putMVar resultVar "完了"

    _ <- forkIO $ safeTask "たろう" taroResult
    _ <- forkIO $ safeTask "はなこ" hanakoResult

    taro   <- takeMVar taroResult
    hanako <- takeMVar hanakoResult
    putStrLn $ "\n結果: たろう=" ++ taro ++ ", はなこ=" ++ hanako

-- メイン
main :: IO ()
main = do
    deadlockScenario
    threadDelay 500000
    safeScenario
