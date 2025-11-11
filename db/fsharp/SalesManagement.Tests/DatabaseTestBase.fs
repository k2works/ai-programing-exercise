module SalesManagement.Tests.DatabaseTestBase

open System
open System.Threading.Tasks
open Testcontainers.PostgreSql
open SalesManagement.Infrastructure.MigrationRunner
open SalesManagement.Infrastructure.OptionHandlerRegistration
open Xunit

/// <summary>
/// データベーステスト用の基底クラス
/// Testcontainersを使用してPostgreSQLコンテナを起動し、
/// テスト実行後に自動的にクリーンアップを行います
/// </summary>
type DatabaseTestBase() =
    let mutable container: PostgreSqlContainer = null
    let mutable connectionString: string = null

    /// <summary>
    /// テスト実行前の初期化処理
    /// PostgreSQLコンテナを起動し、マイグレーションを実行します
    /// </summary>
    member this.InitializeAsync() =
        task {
            // Dapper Option TypeHandler の登録
            register()

            // PostgreSQLコンテナの設定と起動
            container <-
                PostgreSqlBuilder()
                    .WithImage("postgres:16-alpine")
                    .WithDatabase("test_db")
                    .WithUsername("test")
                    .WithPassword("test")
                    .Build()

            do! container.StartAsync()

            // 接続文字列の取得
            connectionString <- container.GetConnectionString()

            // マイグレーションの実行
            migrateDatabase connectionString "PostgreSQL"

            return ()
        } :> Task

    /// <summary>
    /// テスト実行後のクリーンアップ処理
    /// コンテナを停止・破棄します
    /// </summary>
    member this.DisposeAsync() =
        task {
            if container <> null then
                do! container.DisposeAsync().AsTask()
            return ()
        } :> Task

    /// <summary>
    /// データベース接続文字列を取得
    /// </summary>
    member this.ConnectionString = connectionString

    interface IAsyncLifetime with
        member this.InitializeAsync() = this.InitializeAsync()
        member this.DisposeAsync() = this.DisposeAsync()
