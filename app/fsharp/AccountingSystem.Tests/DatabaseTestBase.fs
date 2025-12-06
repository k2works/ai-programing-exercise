module AccountingSystem.Tests.DatabaseTestBase

open System.Threading.Tasks
open Testcontainers.PostgreSql
open AccountingSystem.Tests.PostgresContainerHelper
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
            let! (c, cs) = initializePostgresContainerAsync ()
            container <- c
            connectionString <- cs
        } :> Task

    /// <summary>
    /// テスト実行後のクリーンアップ処理
    /// コンテナを停止・破棄します
    /// </summary>
    member this.DisposeAsync() =
        task {
            if container <> null then
                do! container.DisposeAsync().AsTask()
        } :> Task

    /// <summary>
    /// データベース接続文字列を取得
    /// </summary>
    member this.ConnectionString = connectionString

    interface IAsyncLifetime with
        member this.InitializeAsync() = this.InitializeAsync()
        member this.DisposeAsync() = this.DisposeAsync()
