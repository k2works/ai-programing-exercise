namespace AccountingSystem.Infrastructure.Messaging

open System
open System.Text
open System.Text.Json
open System.Threading.Tasks
open RabbitMQ.Client
open AccountingSystem.Domain.Types
open AccountingSystem.Domain.Events
open AccountingSystem.Application.Port.Out

/// <summary>
/// RabbitMQ 設定
/// </summary>
type RabbitMqConfig = {
    HostName: string
    Port: int
    UserName: string
    Password: string
    VirtualHost: string
    ExchangeName: string
}

module RabbitMqConfig =
    let defaultConfig = {
        HostName = "localhost"
        Port = 5672
        UserName = "guest"
        Password = "guest"
        VirtualHost = "/"
        ExchangeName = "accounting.events"
    }

/// <summary>
/// RabbitMQ イベントパブリッシャー
/// ドメインイベントを RabbitMQ に発行する
/// </summary>
type RabbitMqEventPublisher(config: RabbitMqConfig) =
    let mutable connection: IConnection option = None
    let mutable channel: IChannel option = None
    let lockObj = obj()

    /// <summary>
    /// 接続を確立（遅延初期化）
    /// </summary>
    let ensureConnectionAsync () : Task<IChannel> =
        task {
            match channel with
            | Some ch when ch.IsOpen -> return ch
            | _ ->
                lock lockObj (fun () ->
                    match channel with
                    | Some ch when ch.IsOpen -> ()
                    | _ ->
                        // 既存の接続をクリーンアップ
                        channel |> Option.iter (fun ch ->
                            try ch.Dispose() with _ -> ())
                        connection |> Option.iter (fun conn ->
                            try conn.Dispose() with _ -> ())

                        // 新しい接続を作成
                        let factory = ConnectionFactory(
                            HostName = config.HostName,
                            Port = config.Port,
                            UserName = config.UserName,
                            Password = config.Password,
                            VirtualHost = config.VirtualHost
                        )

                        let newConnection = factory.CreateConnectionAsync() |> Async.AwaitTask |> Async.RunSynchronously
                        let newChannel = newConnection.CreateChannelAsync() |> Async.AwaitTask |> Async.RunSynchronously

                        // Exchange を宣言（topic 型で永続化）
                        newChannel.ExchangeDeclareAsync(
                            exchange = config.ExchangeName,
                            ``type`` = ExchangeType.Topic,
                            durable = true,
                            autoDelete = false,
                            arguments = null
                        ) |> Async.AwaitTask |> Async.RunSynchronously |> ignore

                        connection <- Some newConnection
                        channel <- Some newChannel
                )
                return channel.Value
        }

    /// <summary>
    /// LineItem をシリアライズ可能な形式に変換
    /// </summary>
    let serializeLineItem (item: JournalEntryLineItem) =
        {| AccountCode = item.AccountCode
           DebitCredit = match item.DebitCredit with
                         | DebitCreditType.Debit -> "Debit"
                         | DebitCreditType.Credit -> "Credit"
           Amount = item.Amount |}

    /// <summary>
    /// イベントをシリアライズ
    /// </summary>
    let serializeEvent (event: JournalEntryEvent) : byte[] =
        let eventType = JournalEntryEvent.getEventType event
        let aggregateId = JournalEntryEvent.getAggregateId event

        let (dataJson, occurredAt) =
            match event with
            | JournalEntryCreated data ->
                let serializable = {|
                    JournalEntryId = data.JournalEntryId
                    EntryDate = data.EntryDate
                    Description = data.Description
                    LineItems = data.LineItems |> List.map serializeLineItem
                    UserId = data.UserId
                    OccurredAt = data.OccurredAt
                |}
                (JsonSerializer.Serialize(serializable), data.OccurredAt)
            | JournalEntryApproved data ->
                let serializable = {|
                    JournalEntryId = data.JournalEntryId
                    ApprovedBy = data.ApprovedBy
                    ApprovalComment = data.ApprovalComment
                    OccurredAt = data.OccurredAt
                    UserId = data.UserId
                |}
                (JsonSerializer.Serialize(serializable), data.OccurredAt)
            | JournalEntryDeleted data ->
                let serializable = {|
                    JournalEntryId = data.JournalEntryId
                    Reason = data.Reason
                    OccurredAt = data.OccurredAt
                    UserId = data.UserId
                |}
                (JsonSerializer.Serialize(serializable), data.OccurredAt)

        let envelope = {|
            EventType = eventType
            AggregateId = aggregateId
            Data = JsonDocument.Parse(dataJson).RootElement
            OccurredAt = occurredAt
        |}

        let json = JsonSerializer.Serialize(envelope, JsonSerializerOptions(WriteIndented = false))
        Encoding.UTF8.GetBytes(json)

    /// <summary>
    /// ルーティングキーを取得
    /// </summary>
    let getRoutingKey (event: JournalEntryEvent) : string =
        let eventType = JournalEntryEvent.getEventType event
        let aggregateId = JournalEntryEvent.getAggregateId event
        $"journal.{eventType.ToLowerInvariant()}.{aggregateId}"

    interface IEventPublisher with
        /// <summary>
        /// 単一イベントを RabbitMQ に発行
        /// </summary>
        member _.PublishAsync(event: JournalEntryEvent) : Task<unit> =
            task {
                let! ch = ensureConnectionAsync ()

                let body = serializeEvent event
                let routingKey = getRoutingKey event

                let properties = BasicProperties(
                    ContentType = "application/json",
                    DeliveryMode = DeliveryModes.Persistent,
                    Timestamp = AmqpTimestamp(DateTimeOffset.UtcNow.ToUnixTimeSeconds()),
                    MessageId = Guid.NewGuid().ToString(),
                    Type = JournalEntryEvent.getEventType event
                )

                do! ch.BasicPublishAsync(
                    exchange = config.ExchangeName,
                    routingKey = routingKey,
                    mandatory = false,
                    basicProperties = properties,
                    body = ReadOnlyMemory<byte>(body)
                )
            }

        /// <summary>
        /// 複数イベントを RabbitMQ に発行
        /// </summary>
        member this.PublishManyAsync(events: JournalEntryEvent list) : Task<unit> =
            task {
                for event in events do
                    do! (this :> IEventPublisher).PublishAsync(event)
            }

    interface IDisposable with
        member _.Dispose() =
            channel |> Option.iter (fun ch ->
                try ch.Dispose() with _ -> ())
            connection |> Option.iter (fun conn ->
                try conn.Dispose() with _ -> ())
            channel <- None
            connection <- None
