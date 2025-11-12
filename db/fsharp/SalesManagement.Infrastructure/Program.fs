open System
open System.IO
open Microsoft.Extensions.Configuration
open SalesManagement.Infrastructure.MigrationRunner
open SalesManagement.Infrastructure.DataSeeder

[<EntryPoint>]
let main argv =
    let configuration =
        ConfigurationBuilder()
            .SetBasePath(Directory.GetCurrentDirectory())
            .AddJsonFile("appsettings.json", optional = false)
            .Build()

    let databaseType =
        match configuration.["DatabaseType"] with
        | null -> "PostgreSQL"
        | dbType -> dbType

    let connectionString = configuration.GetConnectionString(databaseType)

    if String.IsNullOrEmpty(connectionString) then
        printfn $"接続文字列が見つかりません: {databaseType}"
        1
    else
        try
            // コマンドライン引数でモードを判定 (デフォルトはmigrate)
            let mode =
                if argv.Length > 0 then argv.[0]
                else "migrate"

            match mode.ToLower() with
            | "seed" ->
                // Seedデータのみ投入
                printfn "Seedデータ投入開始"
                seedAllAsync connectionString
                |> Async.AwaitTask
                |> Async.RunSynchronously
                printfn "Seedデータ投入完了"
                0

            | "migrate-and-seed" ->
                // マイグレーション実行後にSeeder実行
                printfn $"データベースマイグレーション開始: {databaseType}"
                migrateDatabase connectionString databaseType
                printfn "マイグレーション完了"

                printfn "\nSeedデータ投入開始"
                seedAllAsync connectionString
                |> Async.AwaitTask
                |> Async.RunSynchronously
                printfn "Seedデータ投入完了"
                0

            | _ ->
                // デフォルト: マイグレーションのみ
                printfn $"データベースマイグレーション開始: {databaseType}"
                migrateDatabase connectionString databaseType
                printfn "マイグレーション完了"
                0
        with ex ->
            printfn $"エラー: {ex.Message}"
            printfn $"スタックトレース: {ex.StackTrace}"
            1
