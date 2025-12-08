open System
open System.IO
open Microsoft.Extensions.Configuration
open FinancialAccounting.Infrastructure.MigrationRunner

[<EntryPoint>]
let main argv =
    let configuration =
        ConfigurationBuilder()
            .SetBasePath(Directory.GetCurrentDirectory())
            .AddJsonFile("appsettings.json", optional = false)
            .Build()

    let connectionString = configuration.GetConnectionString("FinancialAccounting")

    if String.IsNullOrEmpty(connectionString) then
        printfn "接続文字列が見つかりません: FinancialAccounting"
        1
    else
        printfn "データベースマイグレーション開始"
        try
            migrateDatabase connectionString
            printfn "マイグレーション完了"
            0
        with ex ->
            printfn $"マイグレーションエラー: {ex.Message}"
            1
