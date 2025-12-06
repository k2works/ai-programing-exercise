module AccountingSystem.Tests.Domain.JournalEntryAggregateTest

open System
open Xunit
open FsUnit.Xunit
open AccountingSystem.Domain.Types
open AccountingSystem.Domain.Events
open AccountingSystem.Domain.Aggregates

/// <summary>
/// 仕訳 Aggregate の単体テスト
/// </summary>
type JournalEntryAggregateTest() =

    /// テスト用の仕訳明細を作成
    let createTestLineItems () =
        [
            { AccountCode = "1000"; DebitCredit = DebitCreditType.Debit; Amount = 10000m }
            { AccountCode = "2000"; DebitCredit = DebitCreditType.Credit; Amount = 10000m }
        ]

    [<Fact>]
    member _.``仕訳を作成できる``() =
        // Arrange
        let id = "J-2024-0001"
        let entryDate = DateTime(2024, 1, 15)
        let description = "売上計上"
        let lineItems = createTestLineItems ()
        let userId = "user1"

        // Act
        let result = JournalEntryAggregate.create id entryDate description lineItems userId

        // Assert
        match result with
        | Ok aggregate ->
            aggregate.Id |> should equal id
            aggregate.EntryDate |> should equal entryDate
            aggregate.Description |> should equal description
            aggregate.LineItems |> should haveLength 2
            aggregate.Status |> should equal JournalEntryStatus.Draft
            aggregate.Deleted |> should equal false
            aggregate.UncommittedEvents |> should haveLength 1
        | Error msg ->
            failwith $"Expected Ok but got Error: {msg}"

    [<Fact>]
    member _.``空の明細で仕訳を作成するとエラーになる``() =
        // Arrange
        let id = "J-2024-0002"
        let entryDate = DateTime(2024, 1, 15)
        let description = "空の明細"
        let lineItems = []
        let userId = "user1"

        // Act
        let result = JournalEntryAggregate.create id entryDate description lineItems userId

        // Assert
        match result with
        | Error msg ->
            Assert.Contains("仕訳明細が必要です", msg)
        | Ok _ ->
            failwith "Expected Error but got Ok"

    [<Fact>]
    member _.``貸借不一致の仕訳を作成するとエラーになる``() =
        // Arrange
        let id = "J-2024-0003"
        let entryDate = DateTime(2024, 1, 15)
        let description = "貸借不一致"
        let lineItems = [
            { AccountCode = "1000"; DebitCredit = DebitCreditType.Debit; Amount = 10000m }
            { AccountCode = "2000"; DebitCredit = DebitCreditType.Credit; Amount = 5000m }
        ]
        let userId = "user1"

        // Act
        let result = JournalEntryAggregate.create id entryDate description lineItems userId

        // Assert
        match result with
        | Error msg ->
            Assert.Contains("借方合計と貸方合計が一致しません", msg)
        | Ok _ ->
            failwith "Expected Error but got Ok"

    [<Fact>]
    member _.``仕訳を承認できる``() =
        // Arrange
        let lineItems = createTestLineItems ()
        let createResult = JournalEntryAggregate.create "J-2024-0004" (DateTime(2024, 1, 15)) "承認テスト" lineItems "user1"
        let aggregate = match createResult with Ok a -> a | Error _ -> failwith "Create failed"

        // Act
        let approvedAggregate = aggregate |> JournalEntryAggregate.markEventsAsCommitted
        let result = JournalEntryAggregate.approve "manager" "承認しました" approvedAggregate

        // Assert
        match result with
        | Ok approved ->
            approved.Status |> should equal JournalEntryStatus.Approved
            approved.UncommittedEvents |> should haveLength 1
        | Error msg ->
            failwith $"Expected Ok but got Error: {msg}"

    [<Fact>]
    member _.``承認済み仕訳を再度承認するとエラーになる``() =
        // Arrange
        let lineItems = createTestLineItems ()
        let createResult = JournalEntryAggregate.create "J-2024-0005" (DateTime(2024, 1, 15)) "二重承認テスト" lineItems "user1"
        let aggregate = match createResult with Ok a -> a | Error _ -> failwith "Create failed"

        let approved = aggregate
                       |> JournalEntryAggregate.markEventsAsCommitted
                       |> JournalEntryAggregate.approve "manager" "承認1"
                       |> Result.map JournalEntryAggregate.markEventsAsCommitted

        // Act
        let result = approved |> Result.bind (JournalEntryAggregate.approve "manager2" "承認2")

        // Assert
        match result with
        | Error msg ->
            Assert.Contains("すでに承認済みです", msg)
        | Ok _ ->
            failwith "Expected Error but got Ok"

    [<Fact>]
    member _.``仕訳を削除できる``() =
        // Arrange
        let lineItems = createTestLineItems ()
        let createResult = JournalEntryAggregate.create "J-2024-0006" (DateTime(2024, 1, 15)) "削除テスト" lineItems "user1"
        let aggregate = match createResult with Ok a -> a | Error _ -> failwith "Create failed"

        // Act
        let deletedAggregate = aggregate |> JournalEntryAggregate.markEventsAsCommitted
        let result = JournalEntryAggregate.delete "テスト削除" "user1" deletedAggregate

        // Assert
        match result with
        | Ok deleted ->
            deleted.Deleted |> should equal true
            deleted.UncommittedEvents |> should haveLength 1
        | Error msg ->
            failwith $"Expected Ok but got Error: {msg}"

    [<Fact>]
    member _.``削除済み仕訳を再度削除するとエラーになる``() =
        // Arrange
        let lineItems = createTestLineItems ()
        let createResult = JournalEntryAggregate.create "J-2024-0007" (DateTime(2024, 1, 15)) "二重削除テスト" lineItems "user1"
        let aggregate = match createResult with Ok a -> a | Error _ -> failwith "Create failed"

        let deleted = aggregate
                      |> JournalEntryAggregate.markEventsAsCommitted
                      |> JournalEntryAggregate.delete "削除1" "user1"
                      |> Result.map JournalEntryAggregate.markEventsAsCommitted

        // Act
        let result = deleted |> Result.bind (JournalEntryAggregate.delete "削除2" "user1")

        // Assert
        match result with
        | Error msg ->
            Assert.Contains("すでに削除済みです", msg)
        | Ok _ ->
            failwith "Expected Error but got Ok"

    [<Fact>]
    member _.``削除済み仕訳を承認するとエラーになる``() =
        // Arrange
        let lineItems = createTestLineItems ()
        let createResult = JournalEntryAggregate.create "J-2024-0008" (DateTime(2024, 1, 15)) "削除後承認テスト" lineItems "user1"
        let aggregate = match createResult with Ok a -> a | Error _ -> failwith "Create failed"

        let deleted = aggregate
                      |> JournalEntryAggregate.markEventsAsCommitted
                      |> JournalEntryAggregate.delete "削除" "user1"
                      |> Result.map JournalEntryAggregate.markEventsAsCommitted

        // Act
        let result = deleted |> Result.bind (JournalEntryAggregate.approve "manager" "承認")

        // Assert
        match result with
        | Error msg ->
            Assert.Contains("削除済みの仕訳は承認できません", msg)
        | Ok _ ->
            failwith "Expected Error but got Ok"

    [<Fact>]
    member _.``イベント再生で状態を復元できる``() =
        // Arrange
        let events : JournalEntryEvent list = [
            JournalEntryCreated {
                JournalEntryId = "J-2024-0009"
                EntryDate = DateTime(2024, 1, 15)
                Description = "イベント再生テスト"
                LineItems = createTestLineItems ()
                UserId = "user1"
                OccurredAt = DateTime(2024, 1, 15, 10, 0, 0)
            }
            JournalEntryApproved {
                JournalEntryId = "J-2024-0009"
                ApprovedBy = "manager"
                ApprovalComment = "承認しました"
                OccurredAt = DateTime(2024, 1, 15, 11, 0, 0)
                UserId = "manager"
            }
        ]

        // Act
        let aggregate = JournalEntryAggregate.replay events

        // Assert
        aggregate.Id |> should equal "J-2024-0009"
        aggregate.Description |> should equal "イベント再生テスト"
        aggregate.Status |> should equal JournalEntryStatus.Approved
        aggregate.Deleted |> should equal false
        aggregate.Version |> should equal 2

    [<Fact>]
    member _.``削除イベントを含むイベント再生``() =
        // Arrange
        let events : JournalEntryEvent list = [
            JournalEntryCreated {
                JournalEntryId = "J-2024-0010"
                EntryDate = DateTime(2024, 1, 15)
                Description = "削除再生テスト"
                LineItems = createTestLineItems ()
                UserId = "user1"
                OccurredAt = DateTime(2024, 1, 15, 10, 0, 0)
            }
            JournalEntryDeleted {
                JournalEntryId = "J-2024-0010"
                Reason = "入力ミス"
                OccurredAt = DateTime(2024, 1, 15, 11, 0, 0)
                UserId = "user1"
            }
        ]

        // Act
        let aggregate = JournalEntryAggregate.replay events

        // Assert
        aggregate.Id |> should equal "J-2024-0010"
        aggregate.Deleted |> should equal true
        aggregate.Version |> should equal 2

    [<Fact>]
    member _.``借方合計を正しく計算できる``() =
        // Arrange
        let lineItems = [
            { AccountCode = "1000"; DebitCredit = DebitCreditType.Debit; Amount = 10000m }
            { AccountCode = "1100"; DebitCredit = DebitCreditType.Debit; Amount = 5000m }
            { AccountCode = "2000"; DebitCredit = DebitCreditType.Credit; Amount = 15000m }
        ]
        let createResult = JournalEntryAggregate.create "J-2024-0011" (DateTime(2024, 1, 15)) "合計テスト" lineItems "user1"
        let aggregate = match createResult with Ok a -> a | Error _ -> failwith "Create failed"

        // Act
        let debitSum = JournalEntryAggregate.sumDebit aggregate

        // Assert
        debitSum |> should equal 15000m

    [<Fact>]
    member _.``貸方合計を正しく計算できる``() =
        // Arrange
        let lineItems = [
            { AccountCode = "1000"; DebitCredit = DebitCreditType.Debit; Amount = 15000m }
            { AccountCode = "2000"; DebitCredit = DebitCreditType.Credit; Amount = 10000m }
            { AccountCode = "2100"; DebitCredit = DebitCreditType.Credit; Amount = 5000m }
        ]
        let createResult = JournalEntryAggregate.create "J-2024-0012" (DateTime(2024, 1, 15)) "合計テスト2" lineItems "user1"
        let aggregate = match createResult with Ok a -> a | Error _ -> failwith "Create failed"

        // Act
        let creditSum = JournalEntryAggregate.sumCredit aggregate

        // Assert
        creditSum |> should equal 15000m
