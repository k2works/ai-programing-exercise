namespace AccountingSystem.Infrastructure.Messaging

open System
open System.Text
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open RabbitMQ.Client
open RabbitMQ.Client.Events
open AccountingSystem.Domain.Events
open AccountingSystem.Domain.Types
open AccountingSystem.Application.Port.In

/// <summary>
/// RabbitMQ イベントサブスクライバー
/// RabbitMQ からイベントを受信してハンドラーに配信する
/// </summary>
type RabbitMqEventSubscriber(config: RabbitMqConfig, handlers: IJournalEntryEventHandler seq) =
    let mutable connection: IConnection option = None
    let mutable channel: IChannel option = None
    let mutable consumerTag: string option = None
    let handlerList = handlers |> Seq.toList
    let lockObj = obj()

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
            printfn $"イベントデシリアライズエラー: {ex.Message}"
            None

    /// <summary>
    /// イベントをハンドラーに配信
    /// </summary>
    let dispatchEventAsync (event: JournalEntryEvent) : Task<bool> =
        task {
            let eventType = JournalEntryEvent.getEventType event
            let targetHandlers =
                handlerList
                |> List.filter (fun h -> h.EventTypes |> List.contains eventType)

            let mutable allSuccess = true

            for handler in targetHandlers do
                let! result = handler.HandleAsync(event)
                match result with
                | Ok () -> ()
                | Error msg ->
                    printfn $"ハンドラーエラー: {msg}"
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
                    let! success = dispatchEventAsync event
                    if success then
                        // 成功時は ACK
                        do! ch.BasicAckAsync(args.DeliveryTag, multiple = false)
                    else
                        // 失敗時は NACK（再キュー）
                        do! ch.BasicNackAsync(args.DeliveryTag, multiple = false, requeue = true)
                | None ->
                    // デシリアライズ失敗時は ACK（再処理しても無駄）
                    do! ch.BasicAckAsync(args.DeliveryTag, multiple = false)
            with
            | ex ->
                printfn $"メッセージ処理エラー: {ex.Message}"
                do! ch.BasicNackAsync(args.DeliveryTag, multiple = false, requeue = true)
        }

    /// <summary>
    /// サブスクリプションを開始
    /// </summary>
    member _.StartAsync(cancellationToken: CancellationToken) : Task<unit> =
        task {
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
        }

    /// <summary>
    /// サブスクリプションを停止
    /// </summary>
    member _.StopAsync() : Task<unit> =
        task {
            match channel, consumerTag with
            | Some ch, Some tag when ch.IsOpen ->
                do! ch.BasicCancelAsync(tag)
                consumerTag <- None
            | _ -> ()
        }

    interface IDisposable with
        member _.Dispose() =
            channel |> Option.iter (fun ch ->
                try ch.Dispose() with _ -> ())
            connection |> Option.iter (fun conn ->
                try conn.Dispose() with _ -> ())
            channel <- None
            connection <- None
            consumerTag <- None
