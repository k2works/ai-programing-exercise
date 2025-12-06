namespace AccountingSystem.Infrastructure.Messaging

open System
open System.Text
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open RabbitMQ.Client
open RabbitMQ.Client.Events
open AccountingSystem.Domain.Events
open AccountingSystem.Domain.Types
open AccountingSystem.Application.Port.In

/// <summary>
/// RabbitMQ イベントサブスクライバー
/// アプリケーション起動時に RabbitMQ からイベントを購読し、
/// Scoped なハンドラーを使用して処理する
/// </summary>
type RabbitMqEventSubscriber
    (
        config: RabbitMqConfig,
        serviceScopeFactory: IServiceScopeFactory,
        logger: ILogger<RabbitMqEventSubscriber>
    ) =
    inherit BackgroundService()

    let mutable connection: IConnection option = None
    let mutable channel: IChannel option = None
    let mutable consumerTag: string option = None
    let queueName = "accounting.journal-entry.events"

    /// <summary>
    /// 接続を確立
    /// </summary>
    let connectAsync () : Task<IChannel> =
        task {
            match channel with
            | Some ch when ch.IsOpen -> return ch
            | _ ->
                // 既存の接続をクリーンアップ
                channel |> Option.iter (fun ch ->
                    try ch.Dispose() with _ -> ())
                connection |> Option.iter (fun conn ->
                    try conn.Dispose() with _ -> ())

                let factory = ConnectionFactory(
                    HostName = config.HostName,
                    Port = config.Port,
                    UserName = config.UserName,
                    Password = config.Password,
                    VirtualHost = config.VirtualHost
                )

                logger.LogInformation("RabbitMQ に接続中: {Host}:{Port}", config.HostName, config.Port)

                let! newConnection = factory.CreateConnectionAsync()
                let! newChannel = newConnection.CreateChannelAsync()

                // Exchange を宣言
                do! newChannel.ExchangeDeclareAsync(
                    exchange = config.ExchangeName,
                    ``type`` = ExchangeType.Topic,
                    durable = true,
                    autoDelete = false,
                    arguments = null
                )

                // Queue を宣言（永続化）
                let! _ = newChannel.QueueDeclareAsync(
                    queue = queueName,
                    durable = true,
                    exclusive = false,
                    autoDelete = false,
                    arguments = null
                )

                // Queue を Exchange にバインド（すべての journal イベントを購読）
                do! newChannel.QueueBindAsync(
                    queue = queueName,
                    exchange = config.ExchangeName,
                    routingKey = "journal.#"
                )

                // QoS 設定（プリフェッチ数を制限）
                do! newChannel.BasicQosAsync(prefetchSize = 0u, prefetchCount = 10us, ``global`` = false)

                connection <- Some newConnection
                channel <- Some newChannel

                logger.LogInformation("RabbitMQ 接続完了。キュー '{Queue}' を購読中", queueName)

                return newChannel
        }

    /// <summary>
    /// JSON からイベントをデシリアライズ
    /// </summary>
    let deserializeEvent (body: ReadOnlyMemory<byte>) : JournalEntryEvent option =
        try
            let json = Encoding.UTF8.GetString(body.Span)
            let doc = JsonDocument.Parse(json)
            let root = doc.RootElement

            let eventType = root.GetProperty("EventType").GetString()
            let dataElement = root.GetProperty("Data")

            match eventType with
            | "JournalEntryCreated" ->
                let lineItems =
                    dataElement.GetProperty("LineItems").EnumerateArray()
                    |> Seq.map (fun item ->
                        {
                            AccountCode = item.GetProperty("AccountCode").GetString()
                            DebitCredit =
                                match item.GetProperty("DebitCredit").GetString() with
                                | "Debit" -> DebitCreditType.Debit
                                | _ -> DebitCreditType.Credit
                            Amount = item.GetProperty("Amount").GetDecimal()
                        } : JournalEntryLineItem)
                    |> Seq.toList

                Some (JournalEntryCreated {
                    JournalEntryId = dataElement.GetProperty("JournalEntryId").GetString()
                    EntryDate = dataElement.GetProperty("EntryDate").GetDateTime()
                    Description = dataElement.GetProperty("Description").GetString()
                    LineItems = lineItems
                    UserId = dataElement.GetProperty("UserId").GetString()
                    OccurredAt = dataElement.GetProperty("OccurredAt").GetDateTime()
                })

            | "JournalEntryApproved" ->
                Some (JournalEntryApproved {
                    JournalEntryId = dataElement.GetProperty("JournalEntryId").GetString()
                    ApprovedBy = dataElement.GetProperty("ApprovedBy").GetString()
                    ApprovalComment = dataElement.GetProperty("ApprovalComment").GetString()
                    OccurredAt = dataElement.GetProperty("OccurredAt").GetDateTime()
                    UserId = dataElement.GetProperty("UserId").GetString()
                })

            | "JournalEntryDeleted" ->
                Some (JournalEntryDeleted {
                    JournalEntryId = dataElement.GetProperty("JournalEntryId").GetString()
                    Reason = dataElement.GetProperty("Reason").GetString()
                    OccurredAt = dataElement.GetProperty("OccurredAt").GetDateTime()
                    UserId = dataElement.GetProperty("UserId").GetString()
                })

            | _ -> None
        with
        | ex ->
            logger.LogError(ex, "イベントデシリアライズエラー")
            None

    /// <summary>
    /// イベントをハンドラーに配信（Scoped サービスを使用）
    /// </summary>
    let dispatchEventAsync (event: JournalEntryEvent) : Task<bool> =
        task {
            use scope = serviceScopeFactory.CreateScope()
            let handlers = scope.ServiceProvider.GetServices<IJournalEntryEventHandler>()
            let eventType = JournalEntryEvent.getEventType event

            let mutable allSuccess = true

            for handler in handlers do
                if handler.EventTypes |> List.contains eventType then
                    try
                        let! result = handler.HandleAsync(event)
                        match result with
                        | Ok () ->
                            logger.LogDebug("ハンドラー処理成功: {EventType}", eventType)
                        | Error msg ->
                            logger.LogWarning("ハンドラーエラー: {Error}", msg)
                            allSuccess <- false
                    with
                    | ex ->
                        logger.LogError(ex, "ハンドラー例外: {EventType}", eventType)
                        allSuccess <- false

            return allSuccess
        }

    /// <summary>
    /// メッセージ受信ハンドラー
    /// </summary>
    let handleMessageAsync (ch: IChannel) (args: BasicDeliverEventArgs) : Task =
        task {
            try
                match deserializeEvent args.Body with
                | Some event ->
                    logger.LogInformation(
                        "イベント受信: {EventType}, ID: {JournalEntryId}",
                        JournalEntryEvent.getEventType event,
                        JournalEntryEvent.getAggregateId event
                    )

                    let! success = dispatchEventAsync event
                    if success then
                        // 成功時は ACK
                        do! ch.BasicAckAsync(args.DeliveryTag, multiple = false)
                        logger.LogDebug("メッセージ ACK 完了")
                    else
                        // 失敗時は NACK（再キュー）
                        do! ch.BasicNackAsync(args.DeliveryTag, multiple = false, requeue = true)
                        logger.LogWarning("メッセージ NACK（再キュー）")
                | None ->
                    // デシリアライズ失敗時は ACK（再処理しても無駄）
                    do! ch.BasicAckAsync(args.DeliveryTag, multiple = false)
                    logger.LogWarning("不明なメッセージ形式のため ACK")
            with
            | ex ->
                logger.LogError(ex, "メッセージ処理エラー")
                do! ch.BasicNackAsync(args.DeliveryTag, multiple = false, requeue = true)
        }

    override _.ExecuteAsync(stoppingToken: CancellationToken) : Task =
        task {
            logger.LogInformation("RabbitMQ イベントサブスクライバー開始")

            try
                let! ch = connectAsync ()

                let consumer = AsyncEventingBasicConsumer(ch)

                consumer.add_ReceivedAsync(
                    AsyncEventHandler<BasicDeliverEventArgs>(fun _ args ->
                        handleMessageAsync ch args
                    )
                )

                let! tag = ch.BasicConsumeAsync(
                    queue = queueName,
                    autoAck = false,
                    consumer = consumer
                )

                consumerTag <- Some tag
                logger.LogInformation("RabbitMQ コンシューマー開始: {ConsumerTag}", tag)

                // キャンセルされるまで待機
                while not stoppingToken.IsCancellationRequested do
                    do! Task.Delay(1000, stoppingToken)
            with
            | :? OperationCanceledException ->
                logger.LogInformation("RabbitMQ イベントサブスクライバー停止要求")
            | ex ->
                logger.LogError(ex, "RabbitMQ イベントサブスクライバーエラー")
        }

    /// <summary>
    /// クリーンアップ処理
    /// </summary>
    member private _.CleanupAsync() : Task =
        task {
            logger.LogInformation("RabbitMQ イベントサブスクライバー停止中...")

            match channel, consumerTag with
            | Some ch, Some tag when ch.IsOpen ->
                try
                    do! ch.BasicCancelAsync(tag)
                    consumerTag <- None
                    logger.LogInformation("コンシューマーキャンセル完了")
                with
                | ex -> logger.LogWarning(ex, "コンシューマーキャンセルエラー")
            | _ -> ()

            // 接続をクリーンアップ
            channel |> Option.iter (fun ch ->
                try ch.Dispose() with _ -> ())
            connection |> Option.iter (fun conn ->
                try conn.Dispose() with _ -> ())
            channel <- None
            connection <- None
        }

    override this.StopAsync(cancellationToken: CancellationToken) : Task =
        let cleanup = this.CleanupAsync()
        let baseStop = base.StopAsync(cancellationToken)
        Task.WhenAll(cleanup, baseStop)

    override this.Dispose() =
        channel |> Option.iter (fun ch ->
            try ch.Dispose() with _ -> ())
        connection |> Option.iter (fun conn ->
            try conn.Dispose() with _ -> ())
        base.Dispose()
