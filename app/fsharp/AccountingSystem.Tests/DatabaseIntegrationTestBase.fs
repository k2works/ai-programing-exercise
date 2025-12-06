module AccountingSystem.Tests.DatabaseIntegrationTestBase

open System.Threading.Tasks
open DotNet.Testcontainers.Builders
open Testcontainers.PostgreSql
open Testcontainers.RabbitMq
open AccountingSystem.Infrastructure.MigrationRunner
open AccountingSystem.Infrastructure.Messaging
open AccountingSystem.Tests.PostgresContainerHelper
open Xunit

/// <summary>
/// PostgreSQL + RabbitMQ 統合テスト用の基底クラス
/// Testcontainers を使用して PostgreSQL と RabbitMQ コンテナを起動し、
/// テスト実行後に自動的にクリーンアップを行います
/// </summary>
type DatabaseIntegrationTestBase() =
    let mutable postgresContainer: PostgreSqlContainer = null
    let mutable rabbitMqContainer: RabbitMqContainer = null
    let mutable connectionString: string = null
    let mutable rabbitMqConfig: RabbitMqConfig = RabbitMqConfig.defaultConfig

    /// <summary>
    /// RabbitMQ コンテナを作成
    /// </summary>
    let createRabbitMqContainer () =
        RabbitMqBuilder()
            .WithImage("rabbitmq:3-management-alpine")
            .WithUsername("guest")
            .WithPassword("guest")
            .Build()

    /// <summary>
    /// テスト実行前の初期化処理（内部用）
    /// PostgreSQL と RabbitMQ コンテナを起動し、マイグレーションを実行します
    /// サブクラスから呼び出すための非仮想メソッド
    /// </summary>
    member internal this.InitializeCoreAsync() =
        task {
            configureDockerHost ()

            // コンテナの作成
            postgresContainer <- createPostgresContainer ()
            rabbitMqContainer <- createRabbitMqContainer ()

            // 並行で起動
            do! Task.WhenAll(
                postgresContainer.StartAsync(),
                rabbitMqContainer.StartAsync()
            )

            // PostgreSQL 接続文字列の取得
            connectionString <- postgresContainer.GetConnectionString()

            // RabbitMQ 接続情報の取得
            let rabbitMqHost = rabbitMqContainer.Hostname
            let rabbitMqPort = rabbitMqContainer.GetMappedPublicPort(5672)

            rabbitMqConfig <- {
                HostName = rabbitMqHost
                Port = int rabbitMqPort
                UserName = "guest"
                Password = "guest"
                VirtualHost = "/"
                ExchangeName = "accounting.events.test"
            }

            // マイグレーションの実行
            migrateDatabase connectionString "PostgreSQL"
        } :> Task

    /// <summary>
    /// テスト実行前の初期化処理
    /// PostgreSQL と RabbitMQ コンテナを起動し、マイグレーションを実行します
    /// </summary>
    abstract member InitializeAsync: unit -> Task
    default this.InitializeAsync() = this.InitializeCoreAsync()

    /// <summary>
    /// テスト実行後のクリーンアップ処理
    /// コンテナを停止・破棄します
    /// </summary>
    member this.DisposeAsync() =
        task {
            if rabbitMqContainer <> null then
                do! rabbitMqContainer.DisposeAsync().AsTask()
            if postgresContainer <> null then
                do! postgresContainer.DisposeAsync().AsTask()
        } :> Task

    /// <summary>
    /// データベース接続文字列を取得
    /// </summary>
    member this.ConnectionString = connectionString

    /// <summary>
    /// RabbitMQ 設定を取得
    /// </summary>
    member this.RabbitMqConfig = rabbitMqConfig

    /// <summary>
    /// RabbitMQ 設定を指定した ExchangeName で取得
    /// </summary>
    member this.GetRabbitMqConfigWithExchange(exchangeName: string) =
        { rabbitMqConfig with ExchangeName = exchangeName }

    interface IAsyncLifetime with
        member this.InitializeAsync() = this.InitializeAsync()
        member this.DisposeAsync() = this.DisposeAsync()
