module SearchAlgorithms
  ( linearSearch
  , binarySearch
  , createMap
  , searchInMap
  , addToMap
  , removeFromMap
  ) where

import Data.List (elemIndex)
import qualified Data.Map as Map

-- | 線形探索 - リスト内で要素を検索し、最初に見つかったインデックスを返す
linearSearch :: Eq a => a -> [a] -> Maybe Int
linearSearch key = elemIndex key

-- | 二分探索 - ソートされたリストから指定したキーを検索
-- 注意: リストでの実装は非効率的（学習目的）
binarySearch :: (Ord a) => a -> [a] -> Maybe a
binarySearch _ [] = Nothing
binarySearch key xs =
  let midIndex = length xs `div` 2
      midValue = xs !! midIndex
  in if key == midValue
     then Just midValue
     else if key < midValue
          then binarySearch key (take midIndex xs)
          else binarySearch key (drop (midIndex + 1) xs)

-- | キー・値ペアのリストからMapを作成
createMap :: (Ord k) => [(k, v)] -> Map.Map k v
createMap = Map.fromList

-- | Mapからキーに対応する値を検索
searchInMap :: (Ord k) => k -> Map.Map k v -> Maybe v
searchInMap = Map.lookup

-- | Mapにキー・値ペアを追加または更新
addToMap :: (Ord k) => k -> v -> Map.Map k v -> Map.Map k v
addToMap = Map.insert

-- | Mapからキーとその値を削除
removeFromMap :: (Ord k) => k -> Map.Map k v -> Map.Map k v
removeFromMap = Map.delete
