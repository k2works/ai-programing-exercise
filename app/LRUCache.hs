module LRUCache where

import Data.List (find, filter)

-- | LRUキャッシュの型
data LRUCache k v = LRUCache
    { capacity :: Int
    , items    :: [(k, v)]  -- 先頭が古い、末尾が新しい
    } deriving (Show)

-- | 空のキャッシュを作成
create :: Int -> LRUCache k v
create cap = LRUCache { capacity = cap, items = [] }

-- | キャッシュから値を取得
get :: Eq k => k -> LRUCache k v -> (Maybe v, LRUCache k v)
get key cache =
    case find (\(k, _) -> k == key) (items cache) of
        Just (k, v) ->
            -- 見つかった → 末尾（最新）に移動
            let newItems = Data.List.filter (\(k', _) -> k' /= key) (items cache)
                           ++ [(k, v)]
            in (Just v, cache { items = newItems })
        Nothing ->
            (Nothing, cache)

-- | キャッシュに値を追加/更新
put :: Eq k => k -> v -> LRUCache k v -> LRUCache k v
put key value cache =
    let -- 既存のキーを削除
        filtered = Data.List.filter (\(k, _) -> k /= key) (items cache)
        -- 容量オーバーなら先頭（最古）を削除
        trimmed = if length filtered >= capacity cache
                  then tail filtered
                  else filtered
        -- 末尾（最新）に追加
        newItems = trimmed ++ [(key, value)]
    in cache { items = newItems }

-- | キャッシュの中身をリストで取得
toList :: LRUCache k v -> [(k, v)]
toList = items

-- ===== 使用例 =====
example :: IO ()
example = do
    let cache0 = create 3 :: LRUCache String Int

    let cache1 = put "A" 1 cache0
    putStrLn $ "Aを追加: " ++ show (toList cache1)

    let cache2 = put "B" 2 cache1
    putStrLn $ "Bを追加: " ++ show (toList cache2)

    let cache3 = put "C" 3 cache2
    putStrLn $ "Cを追加: " ++ show (toList cache3)

    let (_, cache4) = get "A" cache3
    putStrLn $ "Aにアクセス: " ++ show (toList cache4)

    let cache5 = put "D" 4 cache4
    putStrLn $ "Dを追加（Bが削除される）: " ++ show (toList cache5)

main :: IO ()
main = example
