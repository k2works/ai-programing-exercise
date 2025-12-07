module LRUCache

/// LRUキャッシュの型（リストで管理：先頭が古い、末尾が新しい）
type LRUCache<'K, 'V when 'K : equality> = {
    Capacity: int
    Items: ('K * 'V) list
}

/// 空のキャッシュを作成
let create capacity : LRUCache<'K, 'V> =
    { Capacity = capacity; Items = [] }

/// キャッシュから値を取得（あれば最新に移動）
let get key cache =
    match List.tryFind (fun (k, _) -> k = key) cache.Items with
    | Some (k, v) ->
        // 見つかった → その要素を末尾（最新）に移動
        let newItems =
            cache.Items
            |> List.filter (fun (k', _) -> k' <> key)
            |> fun items -> items @ [(k, v)]
        Some v, { cache with Items = newItems }
    | None ->
        None, cache

/// キャッシュに値を追加/更新
let put key value cache =
    // まず既存のキーがあれば削除
    let filtered = List.filter (fun (k, _) -> k <> key) cache.Items

    // 容量オーバーなら先頭（最古）を削除
    let trimmed =
        if List.length filtered >= cache.Capacity then
            List.tail filtered  // 先頭を削除
        else
            filtered

    // 末尾（最新）に追加
    { cache with Items = trimmed @ [(key, value)] }

/// キャッシュの中身を表示
let toList cache = cache.Items

// ===== 使用例 =====
let example () =
    let cache = create 3

    let cache = cache |> put "A" 1
    printfn "Aを追加: %A" (toList cache)

    let cache = cache |> put "B" 2
    printfn "Bを追加: %A" (toList cache)

    let cache = cache |> put "C" 3
    printfn "Cを追加: %A" (toList cache)

    let (_, cache) = get "A" cache
    printfn "Aにアクセス: %A" (toList cache)

    let cache = cache |> put "D" 4
    printfn "Dを追加（Bが削除される）: %A" (toList cache)

// 実行
example ()
