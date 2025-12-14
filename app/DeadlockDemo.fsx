open System
open System.Threading

// リソースを表す型
type 資源 =
    | コントローラー
    | リモコン

// タスクの結果
type タスク結果 =
    | 完了 of string
    | デッドロック of string

// リソースマネージャー（排他制御）
type 資源管理者() =
    let コントローラーのロック = new obj()
    let リモコンのロック = new obj()
    let mutable コントローラーの所有者: string option = None
    let mutable リモコンの所有者: string option = None

    member this.取得を試みる(対象資源: 資源, タスク名: string, タイムアウト時間: int) =
        let ロック対象, 所有者を設定 =
            match 対象資源 with
            | コントローラー -> コントローラーのロック, (fun 値 -> コントローラーの所有者 <- 値)
            | リモコン -> リモコンのロック, (fun 値 -> リモコンの所有者 <- 値)

        let 取得成功 = Monitor.TryEnter(ロック対象, タイムアウト時間)
        if 取得成功 then
            所有者を設定 (Some タスク名)
            printfn "  %s: %A を取得！" タスク名 対象資源
        取得成功

    member this.解放する(対象資源: 資源, タスク名: string) =
        let ロック対象, 所有者を設定 =
            match 対象資源 with
            | コントローラー -> コントローラーのロック, (fun 値 -> コントローラーの所有者 <- 値)
            | リモコン -> リモコンのロック, (fun 値 -> リモコンの所有者 <- 値)

        所有者を設定 None
        Monitor.Exit(ロック対象)
        printfn "  %s: %A を解放" タスク名 対象資源

// デッドロックが起きるパターン（逆順で取得）
let デッドロックシナリオを実行 () =
    printfn "\n=== デッドロックが起きるパターン（逆順）==="
    printfn "たろう: コントローラー → リモコン"
    printfn "はなこ: リモコン → コントローラー\n"

    let 管理者 = 資源管理者()
    let タイムアウト時間 = 2000  // 2秒でタイムアウト

    let たろうのタスク = async {
        printfn "たろう: 開始"
        if 管理者.取得を試みる(コントローラー, "たろう", タイムアウト時間) then
            Thread.Sleep(100)  // 少し待つ（デッドロックを起こすため）
            if 管理者.取得を試みる(リモコン, "たろう", タイムアウト時間) then
                printfn "たろう: ゲーム完了！"
                管理者.解放する(リモコン, "たろう")
                管理者.解放する(コントローラー, "たろう")
                return 完了 "たろう"
            else
                printfn "たろう: リモコンが取れない...タイムアウト！"
                管理者.解放する(コントローラー, "たろう")
                return デッドロック "たろう"
        else
            return デッドロック "たろう"
    }

    let はなこのタスク = async {
        printfn "はなこ: 開始"
        if 管理者.取得を試みる(リモコン, "はなこ", タイムアウト時間) then
            Thread.Sleep(100)
            if 管理者.取得を試みる(コントローラー, "はなこ", タイムアウト時間) then
                printfn "はなこ: ゲーム完了！"
                管理者.解放する(コントローラー, "はなこ")
                管理者.解放する(リモコン, "はなこ")
                return 完了 "はなこ"
            else
                printfn "はなこ: コントローラーが取れない...タイムアウト！"
                管理者.解放する(リモコン, "はなこ")
                return デッドロック "はなこ"
        else
            return デッドロック "はなこ"
    }

    let 結果一覧 =
        [たろうのタスク; はなこのタスク]
        |> Async.Parallel
        |> Async.RunSynchronously

    printfn "\n結果: %A" 結果一覧

// デッドロックを防ぐパターン（同順で取得）
let 安全シナリオを実行 () =
    printfn "\n=== デッドロックを防ぐパターン（同順）==="
    printfn "たろう: コントローラー → リモコン"
    printfn "はなこ: コントローラー → リモコン（同じ順番）\n"

    let 管理者 = 資源管理者()
    let タイムアウト時間 = 5000

    let 安全なタスクを作成 名前 = async {
        printfn "%s: 開始" 名前
        if 管理者.取得を試みる(コントローラー, 名前, タイムアウト時間) then
            Thread.Sleep(100)
            if 管理者.取得を試みる(リモコン, 名前, タイムアウト時間) then
                printfn "%s: ゲーム完了！" 名前
                Thread.Sleep(200)  // ゲームプレイ
                管理者.解放する(リモコン, 名前)
                管理者.解放する(コントローラー, 名前)
                return 完了 名前
            else
                管理者.解放する(コントローラー, 名前)
                return デッドロック 名前
        else
            return デッドロック 名前
    }

    let 結果一覧 =
        [安全なタスクを作成 "たろう"; 安全なタスクを作成 "はなこ"]
        |> Async.Parallel
        |> Async.RunSynchronously

    printfn "\n結果: %A" 結果一覧

// 実行
デッドロックシナリオを実行 ()
Thread.Sleep(500)
安全シナリオを実行 ()
