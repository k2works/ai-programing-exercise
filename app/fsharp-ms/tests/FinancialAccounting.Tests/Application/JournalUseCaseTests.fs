namespace FinancialAccounting.Tests.Application

open System
open System.Threading.Tasks
open Xunit
open FinancialAccounting.Domain.Models
open FinancialAccounting.Application.Ports.In
open FinancialAccounting.Application.Ports.Out
open FinancialAccounting.Application.UseCases

/// <summary>
/// モックリポジトリ
/// </summary>
type MockJournalRepository() =
    let mutable journals: Map<int, Journal> = Map.empty
    let mutable nextId = 1

    interface IJournalRepository with
        member _.SaveAsync(journal: Journal) =
            task {
                let id = nextId
                nextId <- nextId + 1
                let saved = { journal with JournalId = Some id }
                journals <- journals |> Map.add id saved
                return saved
            }

        member _.GetByIdAsync(journalId: int) =
            task {
                return journals |> Map.tryFind journalId
            }

        member _.GetByFiscalYearAsync(fiscalYear: int) =
            task {
                return journals
                    |> Map.toList
                    |> List.map snd
                    |> List.filter (fun j -> j.FiscalYear = fiscalYear)
            }

/// <summary>
/// モックイベントパブリッシャー
/// </summary>
type MockJournalEventPublisher() =
    let mutable createdEvents: Journal list = []
    let mutable updatedEvents: Journal list = []
    let mutable deletedEvents: (int * int) list = []

    member _.CreatedEvents = createdEvents
    member _.UpdatedEvents = updatedEvents
    member _.DeletedEvents = deletedEvents

    interface IJournalEventPublisher with
        member _.PublishJournalCreatedAsync(journal: Journal) =
            task {
                createdEvents <- journal :: createdEvents
            }

        member _.PublishJournalUpdatedAsync(journal: Journal) =
            task {
                updatedEvents <- journal :: updatedEvents
            }

        member _.PublishJournalDeletedAsync(journalId: int, fiscalYear: int) =
            task {
                deletedEvents <- (journalId, fiscalYear) :: deletedEvents
            }

module JournalUseCaseTests =

    [<Fact>]
    let ``仕訳を作成できる`` () =
        task {
            // Arrange
            let repository = MockJournalRepository()
            let eventPublisher = MockJournalEventPublisher()
            let useCase = JournalUseCase(repository, eventPublisher) :> IJournalUseCase

            let request: CreateJournalRequest = {
                JournalDate = DateTime(2024, 1, 15)
                Description = "売上計上"
                FiscalYear = 2024
                Entries = [
                    { AccountCode = "111"; DebitAmount = 1000m; CreditAmount = 0m; Description = "現金" }
                    { AccountCode = "510"; DebitAmount = 0m; CreditAmount = 1000m; Description = "売上" }
                ]
            }

            // Act
            let! result = useCase.CreateJournalAsync(request)

            // Assert
            match result with
            | Ok journal ->
                Assert.True(journal.JournalId.IsSome)
                Assert.Equal("売上計上", journal.Description)
                Assert.Equal(2024, journal.FiscalYear)
                Assert.Equal(2, journal.Entries.Length)
            | Error msg ->
                Assert.Fail($"仕訳作成に失敗: {msg}")
        }

    [<Fact>]
    let ``貸借が一致しない仕訳は作成できない`` () =
        task {
            // Arrange
            let repository = MockJournalRepository()
            let eventPublisher = MockJournalEventPublisher()
            let useCase = JournalUseCase(repository, eventPublisher) :> IJournalUseCase

            let request: CreateJournalRequest = {
                JournalDate = DateTime(2024, 1, 15)
                Description = "不正な仕訳"
                FiscalYear = 2024
                Entries = [
                    { AccountCode = "111"; DebitAmount = 1000m; CreditAmount = 0m; Description = "現金" }
                    { AccountCode = "510"; DebitAmount = 0m; CreditAmount = 500m; Description = "売上（金額不一致）" }
                ]
            }

            // Act
            let! result = useCase.CreateJournalAsync(request)

            // Assert
            match result with
            | Ok _ -> Assert.Fail("エラーになるはず")
            | Error msg -> Assert.Contains("貸借", msg)
        }

    [<Fact>]
    let ``明細がない仕訳は作成できない`` () =
        task {
            // Arrange
            let repository = MockJournalRepository()
            let eventPublisher = MockJournalEventPublisher()
            let useCase = JournalUseCase(repository, eventPublisher) :> IJournalUseCase

            let request: CreateJournalRequest = {
                JournalDate = DateTime(2024, 1, 15)
                Description = "明細なし"
                FiscalYear = 2024
                Entries = []
            }

            // Act
            let! result = useCase.CreateJournalAsync(request)

            // Assert
            match result with
            | Ok _ -> Assert.Fail("エラーになるはず")
            | Error msg -> Assert.Contains("明細", msg)
        }

    [<Fact>]
    let ``IDで仕訳を取得できる`` () =
        task {
            // Arrange
            let repository = MockJournalRepository()
            let eventPublisher = MockJournalEventPublisher()
            let useCase = JournalUseCase(repository, eventPublisher) :> IJournalUseCase

            let request: CreateJournalRequest = {
                JournalDate = DateTime(2024, 1, 15)
                Description = "テスト仕訳"
                FiscalYear = 2024
                Entries = [
                    { AccountCode = "111"; DebitAmount = 1000m; CreditAmount = 0m; Description = "現金" }
                    { AccountCode = "510"; DebitAmount = 0m; CreditAmount = 1000m; Description = "売上" }
                ]
            }
            let! createResult = useCase.CreateJournalAsync(request)
            let journalId = match createResult with Ok j -> j.JournalId.Value | _ -> failwith "作成失敗"

            // Act
            let! result = useCase.GetJournalByIdAsync(journalId)

            // Assert
            Assert.True(result.IsSome)
            Assert.Equal("テスト仕訳", result.Value.Description)
        }

    [<Fact>]
    let ``会計年度で仕訳一覧を取得できる`` () =
        task {
            // Arrange
            let repository = MockJournalRepository()
            let eventPublisher = MockJournalEventPublisher()
            let useCase = JournalUseCase(repository, eventPublisher) :> IJournalUseCase

            let request1: CreateJournalRequest = {
                JournalDate = DateTime(2024, 4, 1)
                Description = "仕訳1"
                FiscalYear = 2024
                Entries = [
                    { AccountCode = "111"; DebitAmount = 1000m; CreditAmount = 0m; Description = "" }
                    { AccountCode = "510"; DebitAmount = 0m; CreditAmount = 1000m; Description = "" }
                ]
            }
            let request2: CreateJournalRequest = {
                JournalDate = DateTime(2024, 5, 1)
                Description = "仕訳2"
                FiscalYear = 2024
                Entries = [
                    { AccountCode = "111"; DebitAmount = 2000m; CreditAmount = 0m; Description = "" }
                    { AccountCode = "510"; DebitAmount = 0m; CreditAmount = 2000m; Description = "" }
                ]
            }
            let! _ = useCase.CreateJournalAsync(request1)
            let! _ = useCase.CreateJournalAsync(request2)

            // Act
            let! journals = useCase.GetJournalsByFiscalYearAsync(2024)

            // Assert
            Assert.Equal(2, journals.Length)
        }
