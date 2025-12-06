module AccountingSystem.Tests.RabbitMqTestBase

open System
open System.Threading.Tasks
open Testcontainers.RabbitMq
open AccountingSystem.Infrastructure.Messaging
open Xunit

/// <summary>
/// RabbitMQ コンテナを使用する統合テストの基底クラス
/// Testcontainers を使用して RabbitMQ コンテナのライフサイクルを管理
/// </summary>
[<AbstractClass>]
type RabbitMqTestBase() =
    let mutable container: RabbitMqContainer = null
    let mutable config: RabbitMqConfig = RabbitMqConfig.defaultConfig

    /// <summary>
    /// RabbitMQ 設定を取得
    /// </summary>
    member _.Config = config

    /// <summary>
    /// RabbitMQ コンテナを取得
    /// </summary>
    member _.Container = container

    /// <summary>
    /// サブクラスで Exchange 名をカスタマイズ可能
    /// </summary>
    abstract member ExchangeName: string
    default _.ExchangeName = "accounting.events.test"

    interface IAsyncLifetime with
        member this.InitializeAsync() : Task =
            task {
                // Docker ホストの設定（Windows Docker Desktop 用）
                let dockerHost =
                    match Environment.GetEnvironmentVariable("DOCKER_HOST") with
                    | null | "" -> "npipe://./pipe/docker_engine"
                    | host -> host

                Environment.SetEnvironmentVariable("DOCKER_HOST", dockerHost)

                // RabbitMQ コンテナの設定と起動
                container <-
                    RabbitMqBuilder()
                        .WithImage("rabbitmq:3-management-alpine")
                        .WithUsername("guest")
                        .WithPassword("guest")
                        .Build()

                do! container.StartAsync()

                // RabbitMQ 接続情報の取得
                let rabbitMqHost = container.Hostname
                let rabbitMqPort = container.GetMappedPublicPort(5672)

                config <- {
                    HostName = rabbitMqHost
                    Port = int rabbitMqPort
                    UserName = "guest"
                    Password = "guest"
                    VirtualHost = "/"
                    ExchangeName = this.ExchangeName
                }
            }

        member _.DisposeAsync() : Task =
            task {
                if container <> null then
                    do! container.DisposeAsync().AsTask()
            }
