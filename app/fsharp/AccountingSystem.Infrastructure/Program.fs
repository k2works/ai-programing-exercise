open System
open System.IO
open Microsoft.Extensions.Configuration
open AccountingSystem.Infrastructure.MigrationRunner

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
        printfn $"データベースマイグレーション開始: {databaseType}"
        try
            migrateDatabase connectionString databaseType
            printfn "マイグレーション完了"
            0
        with ex ->
            printfn $"マイグレーションエラー: {ex.Message}"
            1
