module AccountingSystem.Tests.Application.FinancialStatementServiceTest

open System
open System.Threading.Tasks
open Xunit
open FsUnit.Xunit
open AccountingSystem.Domain.Models
open AccountingSystem.Application.Port.In
open AccountingSystem.Application.Port.Out
open AccountingSystem.Application.Services

/// <summary>
/// モックリポジトリ
/// </summary>
type MockFinancialStatementRepository(balanceSheet: BalanceSheet, incomeStatement: IncomeStatement) =
    interface IFinancialStatementRepository with
        member _.GenerateBalanceSheetAsync(_asOfDate: DateTime) =
            Task.FromResult(balanceSheet)

        member _.GenerateIncomeStatementAsync(_fromDate: DateTime, _toDate: DateTime) =
            Task.FromResult(incomeStatement)

/// <summary>
/// FinancialStatementService のテスト
/// </summary>
type FinancialStatementServiceTest() =

    /// テスト用の貸借対照表を作成
    member private _.CreateBalanceSheet() : BalanceSheet =
        {
            AsOfDate = DateTime(2024, 1, 31)
            Assets = [
                { AccountCode = "1110"; AccountName = "普通預金"; Balance = 8000000M; Percentage = 80M }
                { AccountCode = "1410"; AccountName = "建物"; Balance = 2000000M; Percentage = 20M }
            ]
            Liabilities = [
                { AccountCode = "2110"; AccountName = "買掛金"; Balance = 500000M; Percentage = 5M }
                { AccountCode = "2510"; AccountName = "長期借入金"; Balance = 4500000M; Percentage = 45M }
            ]
            Equity = [
                { AccountCode = "3110"; AccountName = "資本金"; Balance = 5000000M; Percentage = 50M }
            ]
            TotalAssets = 10000000M
            TotalLiabilities = 5000000M
            TotalEquity = 5000000M
            TotalLiabilitiesAndEquity = 10000000M
        }

    /// テスト用の損益計算書を作成
    member private _.CreateIncomeStatement() : IncomeStatement =
        {
            FromDate = DateTime(2024, 1, 1)
            ToDate = DateTime(2024, 1, 31)
            Revenues = [
                { AccountCode = "4110"; AccountName = "売上高"; Balance = 10000000M; Percentage = 100M }
            ]
            Expenses = [
                { AccountCode = "5110"; AccountName = "売上原価"; Balance = 6000000M; Percentage = 60M }
                { AccountCode = "6110"; AccountName = "給与手当"; Balance = 2000000M; Percentage = 20M }
                { AccountCode = "6120"; AccountName = "法定福利費"; Balance = 500000M; Percentage = 5M }
            ]
            GrossProfit = 4000000M
            OperatingIncome = 1500000M
            NetIncome = 1500000M
            TotalRevenues = 10000000M
            TotalExpenses = 8500000M
        }

    [<Fact>]
    member this.``貸借対照表を生成できる``() =
        task {
            // Given
            let balanceSheet = this.CreateBalanceSheet()
            let incomeStatement = this.CreateIncomeStatement()
            let repository = MockFinancialStatementRepository(balanceSheet, incomeStatement)
            let service = FinancialStatementService(repository) :> IFinancialStatementUseCase
            let asOfDate = DateTime(2024, 1, 31)

            // When
            let! result = service.GenerateBalanceSheetAsync(asOfDate)

            // Then
            result.AsOfDate |> should equal asOfDate
            result.TotalAssets |> should equal 10000000M
            result.TotalLiabilities |> should equal 5000000M
            result.TotalEquity |> should equal 5000000M
        }

    [<Fact>]
    member this.``損益計算書を生成できる``() =
        task {
            // Given
            let balanceSheet = this.CreateBalanceSheet()
            let incomeStatement = this.CreateIncomeStatement()
            let repository = MockFinancialStatementRepository(balanceSheet, incomeStatement)
            let service = FinancialStatementService(repository) :> IFinancialStatementUseCase
            let fromDate = DateTime(2024, 1, 1)
            let toDate = DateTime(2024, 1, 31)

            // When
            let! result = service.GenerateIncomeStatementAsync(fromDate, toDate)

            // Then
            result.FromDate |> should equal fromDate
            result.ToDate |> should equal toDate
            result.TotalRevenues |> should equal 10000000M
            result.TotalExpenses |> should equal 8500000M
            result.NetIncome |> should equal 1500000M
        }

    [<Fact>]
    member this.``財務指標を計算できる``() =
        task {
            // Given
            let balanceSheet = this.CreateBalanceSheet()
            let incomeStatement = this.CreateIncomeStatement()
            let repository = MockFinancialStatementRepository(balanceSheet, incomeStatement)
            let service = FinancialStatementService(repository) :> IFinancialStatementUseCase
            let asOfDate = DateTime(2024, 1, 31)
            let fromDate = DateTime(2024, 1, 1)
            let toDate = DateTime(2024, 1, 31)

            // When
            let! ratios = service.CalculateFinancialRatiosAsync(asOfDate, fromDate, toDate)

            // Then
            // 流動比率 = 8,000,000 / 500,000 × 100 = 1600%
            ratios.CurrentRatio |> should equal 1600.00M
            // 自己資本比率 = 5,000,000 / 10,000,000 × 100 = 50%
            ratios.EquityRatio |> should equal 50.00M
            // 売上総利益率 = 4,000,000 / 10,000,000 × 100 = 40%
            ratios.GrossProfitMargin |> should equal 40.00M
            // 営業利益率 = 1,500,000 / 10,000,000 × 100 = 15%
            ratios.OperatingProfitMargin |> should equal 15.00M
            // 当期純利益率 = 1,500,000 / 10,000,000 × 100 = 15%
            ratios.NetProfitMargin |> should equal 15.00M
            // ROA = 1,500,000 / 10,000,000 × 100 = 15%
            ratios.Roa |> should equal 15.00M
            // ROE = 1,500,000 / 5,000,000 × 100 = 30%
            ratios.Roe |> should equal 30.00M
        }
