module AccountingSystem.Tests.Application.JournalServiceTest

open System
open System.Threading.Tasks
open Xunit
open FsUnit.Xunit
open AccountingSystem.Domain.Models.Journal
open AccountingSystem.Domain.Models.JournalLine
open AccountingSystem.Domain.Models.JournalLineItem
open AccountingSystem.Domain.Types
open AccountingSystem.Application.Port.In
open AccountingSystem.Application.Port.Out
open AccountingSystem.Application.Services
open AccountingSystem.Application.Exceptions

/// <summary>
/// モックリポジトリ
/// </summary>
type MockJournalRepository(journals: Journal list ref) =

    interface IJournalRepository with
        member _.SaveAsync(journal: Journal) =
            journals := journal :: !journals
            Task.FromResult(journal.VoucherNumber.Number)

        member _.FindByVoucherNumberAsync(voucherNumber: string) =
            let found = !journals |> List.tryFind (fun j -> j.VoucherNumber.Number = voucherNumber)
            Task.FromResult(found)

        member _.FindByDateRangeAsync(fromDate: DateTime, toDate: DateTime) =
            let found = !journals |> List.filter (fun j ->
                j.PostingDate >= fromDate && j.PostingDate <= toDate)
            Task.FromResult(found)

        member _.UpdateAsync(journal: Journal) =
            journals := !journals |> List.map (fun j ->
                if j.VoucherNumber.Number = journal.VoucherNumber.Number then journal else j)
            Task.FromResult(1)

        member _.DeleteAsync(voucherNumber: string) =
            let originalCount = (!journals).Length
            journals := !journals |> List.filter (fun j -> j.VoucherNumber.Number <> voucherNumber)
            let deletedCount = originalCount - (!journals).Length
            Task.FromResult(deletedCount)

/// <summary>
/// JournalService のテスト
/// </summary>
type JournalServiceTest() =

    /// テスト用の仕訳明細項目を作成
    member private _.CreateLineItem(voucherNumber: string, lineNumber: int, debitCreditType: DebitCreditType, accountCode: string, amount: decimal) : JournalLineItem =
        {
            VoucherNumber = VoucherNumber.Create(voucherNumber)
            LineNumber = lineNumber
            DebitCreditType = debitCreditType
            ExchangeRate = 1.0m
            DepartmentCode = None
            ProjectCode = None
            AccountCode = AccountCode.Create(accountCode)
            SubAccountCode = None
            Amount = CurrencyAmount.Create(CurrencyCode.JPY, amount)
            BaseAmount = Money.Create(amount)
            TaxCategory = None
            TaxRate = None
            TaxCalculationType = None
            DueDate = None
            IsCashFlow = false
            SegmentCode = None
            CounterAccountCode = None
            CounterSubAccountCode = None
            MemoCode = None
            MemoContent = None
        }

    /// テスト用の仕訳明細を作成
    member private _.CreateLine(voucherNumber: string, lineNumber: int, description: string, items: JournalLineItem list) : JournalLine =
        {
            VoucherNumber = VoucherNumber.Create(voucherNumber)
            LineNumber = lineNumber
            Description = description
            Items = items
        }

    /// テスト用の仕訳を作成
    member private _.CreateJournal(voucherNumber: string, lines: JournalLine list) : Journal =
        {
            VoucherNumber = VoucherNumber.Create(voucherNumber)
            PostingDate = DateTime(2024, 1, 15)
            EntryDate = DateTime(2024, 1, 15)
            SettlementFlag = SettlementFlag.Normal
            IsSingleEntry = false
            VoucherType = VoucherType.Transfer
            IsRecurring = false
            EmployeeCode = None
            DepartmentCode = None
            RedSlipFlag = RedSlipFlag.Normal
            RedBlackVoucherNumber = None
            Lines = lines
        }

    /// バランスの取れた仕訳を作成
    member private this.CreateBalancedJournal(voucherNumber: string, amount: decimal) : Journal =
        let debitItem = this.CreateLineItem(voucherNumber, 1, DebitCreditType.Debit, "1110", amount)
        let creditItem = this.CreateLineItem(voucherNumber, 1, DebitCreditType.Credit, "4110", amount)
        let line = this.CreateLine(voucherNumber, 1, "テスト取引", [debitItem; creditItem])
        this.CreateJournal(voucherNumber, [line])

    /// テスト用の初期データ
    member private this.CreateInitialJournals() : Journal list =
        [
            { this.CreateBalancedJournal("1001", 100000m) with PostingDate = DateTime(2024, 1, 15) }
            { this.CreateBalancedJournal("1002", 50000m) with PostingDate = DateTime(2024, 1, 20) }
            { this.CreateBalancedJournal("1003", 200000m) with PostingDate = DateTime(2024, 2, 10) }
        ]

    [<Fact>]
    member this.``伝票番号で仕訳を取得できる``() =
        task {
            // Given
            let journals = ref (this.CreateInitialJournals())
            let repository = MockJournalRepository(journals)
            let service = JournalService(repository) :> IJournalUseCase

            // When
            let! result = service.GetJournalByVoucherNumberAsync(1001)

            // Then
            result.VoucherNumber.Number |> should equal "1001"
        }

    [<Fact>]
    member this.``存在しない伝票番号で検索すると例外が発生する``() =
        task {
            // Given
            let journals = ref (this.CreateInitialJournals())
            let repository = MockJournalRepository(journals)
            let service = JournalService(repository) :> IJournalUseCase

            // When & Then
            let! _ = Assert.ThrowsAsync<JournalNotFoundException>(fun () ->
                service.GetJournalByVoucherNumberAsync(9999) :> Task)
            ()
        }

    [<Fact>]
    member this.``日付範囲で仕訳を取得できる``() =
        task {
            // Given
            let journals = ref (this.CreateInitialJournals())
            let repository = MockJournalRepository(journals)
            let service = JournalService(repository) :> IJournalUseCase

            // When
            let! result = service.GetJournalsByDateRangeAsync(DateTime(2024, 1, 1), DateTime(2024, 1, 31))

            // Then
            result |> should haveLength 2
            result |> List.forall (fun j -> j.PostingDate.Month = 1) |> should be True
        }

    [<Fact>]
    member this.``開始日が終了日より後の場合は例外が発生する``() =
        task {
            // Given
            let journals = ref (this.CreateInitialJournals())
            let repository = MockJournalRepository(journals)
            let service = JournalService(repository) :> IJournalUseCase

            // When & Then
            let! _ = Assert.ThrowsAsync<ArgumentException>(fun () ->
                service.GetJournalsByDateRangeAsync(DateTime(2024, 2, 1), DateTime(2024, 1, 1)) :> Task)
            ()
        }

    [<Fact>]
    member this.``新しい仕訳を作成できる``() =
        task {
            // Given
            let journals = ref (this.CreateInitialJournals())
            let repository = MockJournalRepository(journals)
            let service = JournalService(repository) :> IJournalUseCase
            let newJournal = this.CreateBalancedJournal("2001", 75000m)

            // When
            let! result = service.CreateJournalAsync(newJournal)

            // Then
            result.VoucherNumber.Number |> should equal "2001"
        }

    [<Fact>]
    member this.``重複する伝票番号で作成すると例外が発生する``() =
        task {
            // Given
            let journals = ref (this.CreateInitialJournals())
            let repository = MockJournalRepository(journals)
            let service = JournalService(repository) :> IJournalUseCase
            let duplicateJournal = this.CreateBalancedJournal("1001", 30000m)

            // When & Then
            let! _ = Assert.ThrowsAsync<BusinessRuleViolationException>(fun () ->
                service.CreateJournalAsync(duplicateJournal) :> Task)
            ()
        }

    [<Fact>]
    member this.``明細がない仕訳は作成できない``() =
        task {
            // Given
            let journals = ref (this.CreateInitialJournals())
            let repository = MockJournalRepository(journals)
            let service = JournalService(repository) :> IJournalUseCase
            let emptyJournal = this.CreateJournal("2001", [])

            // When & Then
            let! _ = Assert.ThrowsAsync<InvalidJournalEntryException>(fun () ->
                service.CreateJournalAsync(emptyJournal) :> Task)
            ()
        }

    [<Fact>]
    member this.``貸借が一致しない仕訳は作成できない``() =
        task {
            // Given
            let journals = ref (this.CreateInitialJournals())
            let repository = MockJournalRepository(journals)
            let service = JournalService(repository) :> IJournalUseCase

            // 借方 100,000、貸方 50,000 で不一致
            let debitItem = this.CreateLineItem("2001", 1, DebitCreditType.Debit, "1110", 100000m)
            let creditItem = this.CreateLineItem("2001", 1, DebitCreditType.Credit, "4110", 50000m)
            let line = this.CreateLine("2001", 1, "不一致テスト", [debitItem; creditItem])
            let unbalancedJournal = this.CreateJournal("2001", [line])

            // When & Then
            let! _ = Assert.ThrowsAsync<InvalidJournalEntryException>(fun () ->
                service.CreateJournalAsync(unbalancedJournal) :> Task)
            ()
        }

    [<Fact>]
    member this.``仕訳を更新できる``() =
        task {
            // Given
            let journals = ref (this.CreateInitialJournals())
            let repository = MockJournalRepository(journals)
            let service = JournalService(repository) :> IJournalUseCase
            let updatedJournal = { this.CreateBalancedJournal("1001", 150000m) with EmployeeCode = Some "EMP001" }

            // When
            let! result = service.UpdateJournalAsync 1001 updatedJournal

            // Then
            result.EmployeeCode |> should equal (Some "EMP001")
        }

    [<Fact>]
    member this.``存在しない仕訳を更新すると例外が発生する``() =
        task {
            // Given
            let journals = ref (this.CreateInitialJournals())
            let repository = MockJournalRepository(journals)
            let service = JournalService(repository) :> IJournalUseCase
            let journal = this.CreateBalancedJournal("9999", 50000m)

            // When & Then
            let! _ = Assert.ThrowsAsync<JournalNotFoundException>(fun () ->
                service.UpdateJournalAsync 9999 journal :> Task)
            ()
        }

    [<Fact>]
    member this.``仕訳を削除できる``() =
        task {
            // Given
            let journals = ref (this.CreateInitialJournals())
            let repository = MockJournalRepository(journals)
            let service = JournalService(repository) :> IJournalUseCase

            // When
            do! service.DeleteJournalAsync(1001)

            // Then
            let! _ = Assert.ThrowsAsync<JournalNotFoundException>(fun () ->
                service.GetJournalByVoucherNumberAsync(1001) :> Task)
            ()
        }

    [<Fact>]
    member this.``存在しない仕訳を削除すると例外が発生する``() =
        task {
            // Given
            let journals = ref (this.CreateInitialJournals())
            let repository = MockJournalRepository(journals)
            let service = JournalService(repository) :> IJournalUseCase

            // When & Then
            let! _ = Assert.ThrowsAsync<JournalNotFoundException>(fun () ->
                service.DeleteJournalAsync(9999) :> Task)
            ()
        }

    [<Fact>]
    member this.``複数明細の仕訳で貸借が一致すれば作成できる``() =
        task {
            // Given
            let journals = ref (this.CreateInitialJournals())
            let repository = MockJournalRepository(journals)
            let service = JournalService(repository) :> IJournalUseCase

            // 明細1: 借方 60,000、貸方 60,000
            let debitItem1 = this.CreateLineItem("2001", 1, DebitCreditType.Debit, "1110", 60000m)
            let creditItem1 = this.CreateLineItem("2001", 1, DebitCreditType.Credit, "4110", 60000m)
            let line1 = this.CreateLine("2001", 1, "明細1", [debitItem1; creditItem1])

            // 明細2: 借方 40,000、貸方 40,000
            let debitItem2 = this.CreateLineItem("2001", 2, DebitCreditType.Debit, "1110", 40000m)
            let creditItem2 = this.CreateLineItem("2001", 2, DebitCreditType.Credit, "4110", 40000m)
            let line2 = this.CreateLine("2001", 2, "明細2", [debitItem2; creditItem2])

            let multiLineJournal = this.CreateJournal("2001", [line1; line2])

            // When
            let! result = service.CreateJournalAsync(multiLineJournal)

            // Then
            result.Lines |> should haveLength 2
            Journal.sumDebit result |> Money.toDecimal |> should equal 100000m
            Journal.sumCredit result |> Money.toDecimal |> should equal 100000m
        }
