module AccountingSystem.Tests.PostgresWebTestBase

open System
open System.Net.Http
open System.Threading.Tasks
open Microsoft.AspNetCore.Mvc.Testing
open Microsoft.Extensions.DependencyInjection
open Testcontainers.PostgreSql
open AccountingSystem.Api
open AccountingSystem.Tests.PostgresContainerHelper
open Xunit

/// <summary>
/// PostgreSQL + WebApplicationFactory 統合テスト用の基底クラス
/// Testcontainers を使用して PostgreSQL コンテナを起動し、
/// WebApplicationFactory で API サーバーを起動してテストを実行します
/// </summary>
[<AbstractClass>]
type PostgresWebTestBase() =
    let mutable container: PostgreSqlContainer = null
    let mutable connectionString: string = null
    let mutable factory: WebApplicationFactory<Program> = null
    let mutable client: HttpClient = null

    /// <summary>
    /// データベース接続文字列を取得
    /// </summary>
    member _.ConnectionString = connectionString

    /// <summary>
    /// HTTP クライアントを取得
    /// </summary>
    member _.Client = client

    /// <summary>
    /// PostgreSQL コンテナを取得
    /// </summary>
    member _.Container = container

    /// <summary>
    /// サブクラスで DI 設定をカスタマイズするためのメソッド
    /// </summary>
    abstract member ConfigureServices: IServiceCollection * string -> unit

    /// <summary>
    /// テストデータのセットアップ（オプション）
    /// サブクラスでオーバーライドしてテストデータを挿入できます
    /// </summary>
    abstract member SetupTestDataAsync: string -> Task
    default _.SetupTestDataAsync(_: string) = Task.CompletedTask

    /// <summary>
    /// テスト実行前の初期化処理（内部用）
    /// PostgreSQL コンテナを起動し、WebApplicationFactory を設定します
    /// サブクラスから呼び出すための非仮想メソッド
    /// </summary>
    member internal this.InitializeCoreAsync() =
        task {
            // PostgreSQL コンテナの初期化
            let! (c, cs) = initializePostgresContainerAsync ()
            container <- c
            connectionString <- cs

            // テストデータのセットアップ
            do! this.SetupTestDataAsync(connectionString)

            // WebApplicationFactory で API サーバーを起動
            factory <-
                (new WebApplicationFactory<Program>())
                    .WithWebHostBuilder(fun builder ->
                        builder.ConfigureServices(fun services ->
                            this.ConfigureServices(services, connectionString)
                        ) |> ignore
                    )

            client <- factory.CreateClient()
        } :> Task

    /// <summary>
    /// テスト実行前の初期化処理
    /// PostgreSQL コンテナを起動し、WebApplicationFactory を設定します
    /// </summary>
    abstract member InitializeAsync: unit -> Task
    default this.InitializeAsync() = this.InitializeCoreAsync()

    /// <summary>
    /// テスト実行後のクリーンアップ処理
    /// クライアント、ファクトリ、コンテナを破棄します
    /// </summary>
    member _.DisposeAsync() =
        task {
            if client <> null then
                client.Dispose()
            if factory <> null then
                factory.Dispose()
            if container <> null then
                do! container.DisposeAsync().AsTask()
        } :> Task

    interface IAsyncLifetime with
        member this.InitializeAsync() = this.InitializeAsync()
        member this.DisposeAsync() = this.DisposeAsync()
