namespace ManagementAccounting.Tests.Infrastructure

open System
open System.Threading.Tasks
open Xunit
open Testcontainers.PostgreSql
open ManagementAccounting.Infrastructure.MigrationRunner

/// <summary>
/// Docker ホストを設定（Windows Docker Desktop 用）
/// </summary>
module DockerHostConfig =
    let configure () =
        let dockerHost =
            match Environment.GetEnvironmentVariable("DOCKER_HOST") with
            | null | "" -> "npipe://./pipe/docker_engine"
            | host -> host
        Environment.SetEnvironmentVariable("DOCKER_HOST", dockerHost)

/// <summary>
/// PostgreSQL Testcontainer を使用したテストフィクスチャ
/// </summary>
type PostgresTestFixture() =
    let mutable container: PostgreSqlContainer = null
    let mutable connectionString: string = null

    member this.ConnectionString = connectionString

    interface IAsyncLifetime with
        member this.InitializeAsync() =
            task {
                // Windows Docker Desktop 用に環境変数を設定
                DockerHostConfig.configure ()

                // PostgreSQL コンテナを起動
                container <- PostgreSqlBuilder()
                                .WithImage("postgres:16-alpine")
                                .WithDatabase("management_accounting_test")
                                .WithUsername("postgres")
                                .WithPassword("postgres")
                                .Build()

                do! container.StartAsync()
                connectionString <- container.GetConnectionString()

                // FluentMigrator でマイグレーション実行
                migrateDatabase connectionString
            } :> Task

        member this.DisposeAsync() =
            task {
                if container <> null then
                    do! container.DisposeAsync().AsTask()
            } :> Task

/// <summary>
/// xUnit のコレクション定義（テストクラス間でフィクスチャを共有）
/// </summary>
[<CollectionDefinition("PostgresCollection")>]
type PostgresCollection() =
    interface ICollectionFixture<PostgresTestFixture>
