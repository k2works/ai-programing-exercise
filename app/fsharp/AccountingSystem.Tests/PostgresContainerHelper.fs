module AccountingSystem.Tests.PostgresContainerHelper

open System
open System.Threading.Tasks
open DotNet.Testcontainers.Builders
open Testcontainers.PostgreSql
open AccountingSystem.Infrastructure.MigrationRunner

/// <summary>
/// PostgreSQL コンテナの設定と起動を行うヘルパーモジュール
/// 複数のテストクラスで共通のコンテナ初期化ロジックを提供します
/// </summary>

/// <summary>
/// Docker ホストを設定（Windows Docker Desktop 用）
/// </summary>
let configureDockerHost () =
    let dockerHost =
        match Environment.GetEnvironmentVariable("DOCKER_HOST") with
        | null | "" -> "npipe://./pipe/docker_engine"
        | host -> host
    Environment.SetEnvironmentVariable("DOCKER_HOST", dockerHost)

/// <summary>
/// PostgreSQL コンテナを作成
/// </summary>
let createPostgresContainer () =
    PostgreSqlBuilder()
        .WithImage("postgres:16-alpine")
        .WithDatabase("test_db")
        .WithUsername("test")
        .WithPassword("test")
        .Build()

/// <summary>
/// PostgreSQL コンテナを初期化
/// コンテナを起動し、マイグレーションを実行して接続文字列を返します
/// </summary>
let initializePostgresContainerAsync () =
    task {
        configureDockerHost ()

        let container = createPostgresContainer ()
        do! container.StartAsync()

        let connectionString = container.GetConnectionString()
        migrateDatabase connectionString "PostgreSQL"

        return (container, connectionString)
    }
