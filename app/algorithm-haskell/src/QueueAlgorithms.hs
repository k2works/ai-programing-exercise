module QueueAlgorithms
  ( Queue
  , createEmpty
  , qIsEmpty
  , enqueue
  , dequeue
  , dequeueSafe
  , qPeek
  , qPeekSafe
  ) where

-- | 2つのリストでキューを表現
-- inboxは新しい要素が追加される場所（逆順）
-- outboxは要素が取り出される場所（正順）
data Queue a = Queue { inbox :: [a], outbox :: [a] } deriving (Show, Eq)

-- | 空のキューを作成
createEmpty :: Queue a
createEmpty = Queue [] []

-- | キューが空かどうか
qIsEmpty :: Queue a -> Bool
qIsEmpty (Queue [] []) = True
qIsEmpty _ = False

-- | エンキュー - キューの末尾に要素を追加（inboxに追加）
enqueue :: a -> Queue a -> Queue a
enqueue x (Queue i o) = Queue (x:i) o

-- | デキュー - キューの先頭から要素を取り出し、残りのキューを返す（安全でないバージョン）
dequeue :: Queue a -> (a, Queue a)
dequeue (Queue i (o:os)) = (o, Queue i os) -- outboxに要素があれば、それを使う
dequeue (Queue [] []) = error "dequeue from empty queue"
dequeue (Queue i []) = -- outboxが空なら、inboxを反転してoutboxに移す
  let newOutbox = reverse i
  in case newOutbox of
       [] -> error "dequeue from empty queue"
       (x:xs) -> (x, Queue [] xs)

-- | 安全なデキュー - Maybeを使って空キューでのエラーを防ぐ
dequeueSafe :: Queue a -> Maybe (a, Queue a)
dequeueSafe (Queue i (o:os)) = Just (o, Queue i os)
dequeueSafe (Queue [] []) = Nothing
dequeueSafe (Queue i []) =
  let newOutbox = reverse i
  in case newOutbox of
       [] -> Nothing
       (x:xs) -> Just (x, Queue [] xs)

-- | キューの先頭要素を参照する（取り出さない、安全でないバージョン）
qPeek :: Queue a -> a
qPeek (Queue i (o:_)) = o
qPeek (Queue [] []) = error "peek from empty queue"
qPeek (Queue i []) =
  let newOutbox = reverse i
  in case newOutbox of
       [] -> error "peek from empty queue"
       (x:_) -> x

-- | 安全なピーク - Maybeを使って空キューでのエラーを防ぐ
qPeekSafe :: Queue a -> Maybe a
qPeekSafe (Queue i (o:_)) = Just o
qPeekSafe (Queue [] []) = Nothing
qPeekSafe (Queue i []) =
  let newOutbox = reverse i
  in case newOutbox of
       [] -> Nothing
       (x:_) -> Just x
