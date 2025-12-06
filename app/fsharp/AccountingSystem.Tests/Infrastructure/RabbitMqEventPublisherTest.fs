module AccountingSystem.Tests.Infrastructure.RabbitMqEventPublisherTest

open System
open System.Text
open System.Text.Json
open AccountingSystem.Domain.Types
open AccountingSystem.Domain.Events
open Xunit
open FsUnit.Xunit

/// <summary>
/// RabbitMQ イベントパブリッシャーのシリアライズロジックテスト
/// </summary>
type RabbitMqEventPublisherTest() =

    let createTestLineItems () : JournalEntryLineItem list =
        [
            { AccountCode = "1000"; DebitCredit = DebitCreditType.Debit; Amount = 10000m }
            { AccountCode = "2000"; DebitCredit = DebitCreditType.Credit; Amount = 10000m }
        ]

    let createCreatedEvent (id: string) =
        JournalEntryCreated {
            JournalEntryId = id
            EntryDate = DateTime(2024, 1, 15)
            Description = "テスト仕訳"
            LineItems = createTestLineItems ()
            UserId = "user1"
            OccurredAt = DateTime(2024, 1, 15, 10, 30, 0)
        }

    let createApprovedEvent (id: string) =
        JournalEntryApproved {
            JournalEntryId = id
            ApprovedBy = "manager"
            ApprovalComment = "承認"
            OccurredAt = DateTime(2024, 1, 16, 14, 0, 0)
            UserId = "manager"
        }

    let createDeletedEvent (id: string) =
        JournalEntryDeleted {
            JournalEntryId = id
            Reason = "テスト削除"
            OccurredAt = DateTime(2024, 1, 17, 9, 0, 0)
            UserId = "user1"
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
    /// イベントをシリアライズ（Publisher 内部ロジックを再現）
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
    /// ルーティングキーを取得（Publisher 内部ロジックを再現）
    /// </summary>
    let getRoutingKey (event: JournalEntryEvent) : string =
        let eventType = JournalEntryEvent.getEventType event
        let aggregateId = JournalEntryEvent.getAggregateId event
        $"journal.{eventType.ToLowerInvariant()}.{aggregateId}"

    [<Fact>]
    member _.``JournalEntryCreated イベントを正しくシリアライズできる``() =
        // Arrange
        let event = createCreatedEvent "TEST-001"

        // Act
        let bytes = serializeEvent event
        let json = Encoding.UTF8.GetString(bytes)
        let doc = JsonDocument.Parse(json)
        let root = doc.RootElement

        // Assert
        root.GetProperty("EventType").GetString() |> should equal "JournalEntryCreated"
        root.GetProperty("AggregateId").GetString() |> should equal "TEST-001"
        root.GetProperty("Data").GetProperty("JournalEntryId").GetString() |> should equal "TEST-001"
        root.GetProperty("Data").GetProperty("Description").GetString() |> should equal "テスト仕訳"
        root.GetProperty("Data").GetProperty("LineItems").GetArrayLength() |> should equal 2

    [<Fact>]
    member _.``JournalEntryApproved イベントを正しくシリアライズできる``() =
        // Arrange
        let event = createApprovedEvent "TEST-002"

        // Act
        let bytes = serializeEvent event
        let json = Encoding.UTF8.GetString(bytes)
        let doc = JsonDocument.Parse(json)
        let root = doc.RootElement

        // Assert
        root.GetProperty("EventType").GetString() |> should equal "JournalEntryApproved"
        root.GetProperty("AggregateId").GetString() |> should equal "TEST-002"
        root.GetProperty("Data").GetProperty("ApprovedBy").GetString() |> should equal "manager"
        root.GetProperty("Data").GetProperty("ApprovalComment").GetString() |> should equal "承認"

    [<Fact>]
    member _.``JournalEntryDeleted イベントを正しくシリアライズできる``() =
        // Arrange
        let event = createDeletedEvent "TEST-003"

        // Act
        let bytes = serializeEvent event
        let json = Encoding.UTF8.GetString(bytes)
        let doc = JsonDocument.Parse(json)
        let root = doc.RootElement

        // Assert
        root.GetProperty("EventType").GetString() |> should equal "JournalEntryDeleted"
        root.GetProperty("AggregateId").GetString() |> should equal "TEST-003"
        root.GetProperty("Data").GetProperty("Reason").GetString() |> should equal "テスト削除"

    [<Fact>]
    member _.``JournalEntryCreated のルーティングキーが正しい``() =
        // Arrange
        let event = createCreatedEvent "TEST-004"

        // Act
        let routingKey = getRoutingKey event

        // Assert
        routingKey |> should equal "journal.journalentrycreated.TEST-004"

    [<Fact>]
    member _.``JournalEntryApproved のルーティングキーが正しい``() =
        // Arrange
        let event = createApprovedEvent "TEST-005"

        // Act
        let routingKey = getRoutingKey event

        // Assert
        routingKey |> should equal "journal.journalentryapproved.TEST-005"

    [<Fact>]
    member _.``JournalEntryDeleted のルーティングキーが正しい``() =
        // Arrange
        let event = createDeletedEvent "TEST-006"

        // Act
        let routingKey = getRoutingKey event

        // Assert
        routingKey |> should equal "journal.journalentrydeleted.TEST-006"

    [<Fact>]
    member _.``シリアライズされた JSON が正しくデシリアライズできる``() =
        // Arrange
        let event = createCreatedEvent "TEST-ID-001"

        // Act
        let bytes = serializeEvent event
        let json = Encoding.UTF8.GetString(bytes)
        let doc = JsonDocument.Parse(json)
        let root = doc.RootElement

        // Assert - デシリアライズして値を検証
        root.GetProperty("AggregateId").GetString() |> should equal "TEST-ID-001"
        root.GetProperty("Data").GetProperty("JournalEntryId").GetString() |> should equal "TEST-ID-001"

    [<Fact>]
    member _.``OccurredAt がシリアライズに含まれる``() =
        // Arrange
        let event = createCreatedEvent "TEST-007"

        // Act
        let bytes = serializeEvent event
        let json = Encoding.UTF8.GetString(bytes)
        let doc = JsonDocument.Parse(json)
        let root = doc.RootElement

        // Assert
        let occurredAt = root.GetProperty("OccurredAt").GetDateTime()
        occurredAt |> should equal (DateTime(2024, 1, 15, 10, 30, 0))
