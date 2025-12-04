namespace AccountingSystem.Tests.Application

open System
open System.Threading.Tasks
open Xunit
open FsUnit.Xunit
open AccountingSystem.Application.Repositories
open AccountingSystem.Application.Services

/// <summary>
/// IBalanceRepository のモック実装
/// </summary>
type MockBalanceRepository() =
    let mutable dailyBalanceUpdateCalls: DailyBalanceUpdateParams list = []
    let mutable journalItemUpdateCalls: string list = []
    let mutable monthlyConsolidateCalls: MonthlyBalanceConsolidateParams list = []

    /// 日次残高更新の呼び出し履歴を取得
    member this.DailyBalanceUpdateCalls = dailyBalanceUpdateCalls

    /// 仕訳明細からの更新呼び出し履歴を取得
    member this.JournalItemUpdateCalls = journalItemUpdateCalls

    /// 月次集計の呼び出し履歴を取得
    member this.MonthlyConsolidateCalls = monthlyConsolidateCalls

    interface IBalanceRepository with
        member this.UpdateDailyBalanceAsync(param: DailyBalanceUpdateParams) : Task<unit> =
            dailyBalanceUpdateCalls <- dailyBalanceUpdateCalls @ [param]
            Task.FromResult(())

        member this.UpdateBalanceFromJournalItemsAsync(journalNo: string) : Task<unit> =
            journalItemUpdateCalls <- journalItemUpdateCalls @ [journalNo]
            Task.FromResult(())

        member this.ConsolidateMonthlyBalanceAsync(param: MonthlyBalanceConsolidateParams) : Task<unit> =
            monthlyConsolidateCalls <- monthlyConsolidateCalls @ [param]
            Task.FromResult(())

/// <summary>
/// BalanceService のテスト
/// </summary>
type BalanceServiceTest() =

    [<Fact>]
    member this.``UpdateDailyBalanceAsync でリポジトリが正しいパラメータで呼び出される``() =
        task {
            // Given
            let mockRepository = MockBalanceRepository()
            let service = BalanceService(mockRepository)
            let entryDate = DateTime(2025, 1, 15)
            let accountCode = "1020"
            let subAccountCode = Some "001"
            let departmentCode = Some "SALES"
            let projectCode = Some "P001"
            let settlementFlag = Some 0
            let debitAmount = 100000.00M
            let creditAmount = 0.00M

            // When
            do! service.UpdateDailyBalanceAsync(
                entryDate,
                accountCode,
                subAccountCode,
                departmentCode,
                projectCode,
                settlementFlag,
                debitAmount,
                creditAmount)

            // Then
            mockRepository.DailyBalanceUpdateCalls.Length |> should equal 1
            let call = mockRepository.DailyBalanceUpdateCalls.[0]
            call.EntryDate |> should equal entryDate
            call.AccountCode |> should equal accountCode
            call.SubAccountCode |> should equal subAccountCode
            call.DepartmentCode |> should equal departmentCode
            call.ProjectCode |> should equal projectCode
            call.SettlementFlag |> should equal settlementFlag
            call.DebitAmount |> should equal debitAmount
            call.CreditAmount |> should equal creditAmount
        }

    [<Fact>]
    member this.``UpdateDailyBalanceAsync でオプション値が None の場合も正しく処理される``() =
        task {
            // Given
            let mockRepository = MockBalanceRepository()
            let service = BalanceService(mockRepository)
            let entryDate = DateTime(2025, 1, 15)
            let accountCode = "1020"

            // When
            do! service.UpdateDailyBalanceAsync(
                entryDate,
                accountCode,
                None,
                None,
                None,
                None,
                50000.00M,
                0.00M)

            // Then
            mockRepository.DailyBalanceUpdateCalls.Length |> should equal 1
            let call = mockRepository.DailyBalanceUpdateCalls.[0]
            call.SubAccountCode |> should equal None
            call.DepartmentCode |> should equal None
            call.ProjectCode |> should equal None
            call.SettlementFlag |> should equal None
        }

    [<Fact>]
    member this.``UpdateBalanceFromJournalItemsAsync でリポジトリが正しい仕訳番号で呼び出される``() =
        task {
            // Given
            let mockRepository = MockBalanceRepository()
            let service = BalanceService(mockRepository)
            let journalNo = "JV-2025-00001"

            // When
            do! service.UpdateBalanceFromJournalItemsAsync(journalNo)

            // Then
            mockRepository.JournalItemUpdateCalls.Length |> should equal 1
            mockRepository.JournalItemUpdateCalls.[0] |> should equal journalNo
        }

    [<Fact>]
    member this.``ConsolidateMonthlyBalanceAsync でリポジトリが正しいパラメータで呼び出される``() =
        task {
            // Given
            let mockRepository = MockBalanceRepository()
            let service = BalanceService(mockRepository)
            let fiscalYear = 2025
            let month = 1

            // When
            do! service.ConsolidateMonthlyBalanceAsync(fiscalYear, month)

            // Then
            mockRepository.MonthlyConsolidateCalls.Length |> should equal 1
            let call = mockRepository.MonthlyConsolidateCalls.[0]
            call.FiscalYear |> should equal fiscalYear
            call.Month |> should equal month
        }

    [<Fact>]
    member this.``複数回の呼び出しが正しく記録される``() =
        task {
            // Given
            let mockRepository = MockBalanceRepository()
            let service = BalanceService(mockRepository)

            // When: 複数回呼び出し
            do! service.UpdateBalanceFromJournalItemsAsync("JV-001")
            do! service.UpdateBalanceFromJournalItemsAsync("JV-002")
            do! service.UpdateBalanceFromJournalItemsAsync("JV-003")

            // Then
            mockRepository.JournalItemUpdateCalls.Length |> should equal 3
            mockRepository.JournalItemUpdateCalls.[0] |> should equal "JV-001"
            mockRepository.JournalItemUpdateCalls.[1] |> should equal "JV-002"
            mockRepository.JournalItemUpdateCalls.[2] |> should equal "JV-003"
        }

    [<Fact>]
    member this.``異なるメソッドの呼び出しが独立して記録される``() =
        task {
            // Given
            let mockRepository = MockBalanceRepository()
            let service = BalanceService(mockRepository)

            // When: 異なるメソッドを呼び出し
            do! service.UpdateDailyBalanceAsync(
                DateTime(2025, 1, 15),
                "1020",
                None,
                None,
                None,
                None,
                100000.00M,
                0.00M)
            do! service.UpdateBalanceFromJournalItemsAsync("JV-001")
            do! service.ConsolidateMonthlyBalanceAsync(2025, 1)

            // Then: 各メソッドの呼び出しが独立して記録されている
            mockRepository.DailyBalanceUpdateCalls.Length |> should equal 1
            mockRepository.JournalItemUpdateCalls.Length |> should equal 1
            mockRepository.MonthlyConsolidateCalls.Length |> should equal 1
        }
