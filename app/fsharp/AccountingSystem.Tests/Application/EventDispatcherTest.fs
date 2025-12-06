module AccountingSystem.Tests.Application.EventDispatcherTest

open System
open System.Threading.Tasks
open AccountingSystem.Domain.Types
open AccountingSystem.Domain.Events
open AccountingSystem.Application.Port.In
open AccountingSystem.Application.Services
open Xunit
open FsUnit.Xunit

/// <summary>
/// テスト用のモックイベントハンドラー
/// </summary>
type MockEventHandler(eventTypes: string list, shouldFail: bool) =
    let mutable handledEvents = ResizeArray<JournalEntryEvent>()

    member _.HandledEvents = handledEvents |> Seq.toList

    interface IJournalEntryEventHandler with
        member _.EventTypes = eventTypes

        member _.HandleAsync(event: JournalEntryEvent) : Task<Result<unit, string>> =
            task {
                if shouldFail then
                    return Error "ハンドラーエラー"
                else
                    handledEvents.Add(event)
                    return Ok ()
            }

/// <summary>
/// EventDispatcher テスト
/// </summary>
type EventDispatcherTest() =

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
            OccurredAt = DateTime.UtcNow
        }

    let createApprovedEvent (id: string) =
        JournalEntryApproved {
            JournalEntryId = id
            ApprovedBy = "manager"
            ApprovalComment = "承認"
            OccurredAt = DateTime.UtcNow
            UserId = "manager"
        }

    let createDeletedEvent (id: string) =
        JournalEntryDeleted {
            JournalEntryId = id
            Reason = "テスト削除"
            OccurredAt = DateTime.UtcNow
            UserId = "user1"
        }

    [<Fact>]
    member _.``単一イベントをディスパッチできる``() =
        task {
            // Arrange
            let handler = MockEventHandler(["JournalEntryCreated"], false)
            let dispatcher = EventDispatcher([handler])
            let event = createCreatedEvent "TEST-001"

            // Act
            let! result = dispatcher.DispatchAsync(event)

            // Assert
            result.HandlersExecuted |> should equal 1
            result.SuccessCount |> should equal 1
            result.FailureCount |> should equal 0
            handler.HandledEvents |> should haveLength 1
        }

    [<Fact>]
    member _.``複数ハンドラーにディスパッチできる``() =
        task {
            // Arrange
            let handler1 = MockEventHandler(["JournalEntryCreated"], false)
            let handler2 = MockEventHandler(["JournalEntryCreated"], false)
            let dispatcher = EventDispatcher([handler1; handler2])
            let event = createCreatedEvent "TEST-002"

            // Act
            let! result = dispatcher.DispatchAsync(event)

            // Assert
            result.HandlersExecuted |> should equal 2
            result.SuccessCount |> should equal 2
            handler1.HandledEvents |> should haveLength 1
            handler2.HandledEvents |> should haveLength 1
        }

    [<Fact>]
    member _.``イベントタイプでフィルタリングされる``() =
        task {
            // Arrange
            let createdHandler = MockEventHandler(["JournalEntryCreated"], false)
            let approvedHandler = MockEventHandler(["JournalEntryApproved"], false)
            let dispatcher = EventDispatcher([createdHandler; approvedHandler])
            let event = createCreatedEvent "TEST-003"

            // Act
            let! result = dispatcher.DispatchAsync(event)

            // Assert
            result.HandlersExecuted |> should equal 1
            createdHandler.HandledEvents |> should haveLength 1
            approvedHandler.HandledEvents |> should haveLength 0
        }

    [<Fact>]
    member _.``ハンドラーエラーを記録する``() =
        task {
            // Arrange
            let successHandler = MockEventHandler(["JournalEntryCreated"], false)
            let failHandler = MockEventHandler(["JournalEntryCreated"], true)
            let dispatcher = EventDispatcher([successHandler; failHandler])
            let event = createCreatedEvent "TEST-004"

            // Act
            let! result = dispatcher.DispatchAsync(event)

            // Assert
            result.HandlersExecuted |> should equal 2
            result.SuccessCount |> should equal 1
            result.FailureCount |> should equal 1
            result.Errors |> should haveLength 1
            result.Errors.Head |> should equal "ハンドラーエラー"
        }

    [<Fact>]
    member _.``複数イベントをディスパッチできる``() =
        task {
            // Arrange
            let handler = MockEventHandler(["JournalEntryCreated"; "JournalEntryApproved"], false)
            let dispatcher = EventDispatcher([handler])
            let events = [
                createCreatedEvent "TEST-005"
                createApprovedEvent "TEST-005"
            ]

            // Act
            let! results = dispatcher.DispatchManyAsync(events)

            // Assert
            results |> should haveLength 2
            results.[0].SuccessCount |> should equal 1
            results.[1].SuccessCount |> should equal 1
            handler.HandledEvents |> should haveLength 2
        }

    [<Fact>]
    member _.``IEventPublisher として PublishAsync が動作する``() =
        task {
            // Arrange
            let handler = MockEventHandler(["JournalEntryCreated"], false)
            let dispatcher = EventDispatcher([handler])
            let publisher = dispatcher :> AccountingSystem.Application.Port.Out.IEventPublisher
            let event = createCreatedEvent "TEST-006"

            // Act
            do! publisher.PublishAsync(event)

            // Assert
            handler.HandledEvents |> should haveLength 1
        }

    [<Fact>]
    member _.``IEventPublisher として PublishManyAsync が動作する``() =
        task {
            // Arrange
            let handler = MockEventHandler(["JournalEntryCreated"; "JournalEntryDeleted"], false)
            let dispatcher = EventDispatcher([handler])
            let publisher = dispatcher :> AccountingSystem.Application.Port.Out.IEventPublisher
            let events = [
                createCreatedEvent "TEST-007"
                createDeletedEvent "TEST-007"
            ]

            // Act
            do! publisher.PublishManyAsync(events)

            // Assert
            handler.HandledEvents |> should haveLength 2
        }

    [<Fact>]
    member _.``ハンドラーがない場合は何も実行されない``() =
        task {
            // Arrange
            let handler = MockEventHandler(["JournalEntryApproved"], false)
            let dispatcher = EventDispatcher([handler])
            let event = createCreatedEvent "TEST-008"

            // Act
            let! result = dispatcher.DispatchAsync(event)

            // Assert
            result.HandlersExecuted |> should equal 0
            result.SuccessCount |> should equal 0
            handler.HandledEvents |> should haveLength 0
        }

    [<Fact>]
    member _.``EventDispatchResult に正しい情報が含まれる``() =
        task {
            // Arrange
            let handler = MockEventHandler(["JournalEntryCreated"], false)
            let dispatcher = EventDispatcher([handler])
            let event = createCreatedEvent "TEST-009"

            // Act
            let! result = dispatcher.DispatchAsync(event)

            // Assert
            result.EventType |> should equal "JournalEntryCreated"
            result.AggregateId |> should equal "TEST-009"
        }
