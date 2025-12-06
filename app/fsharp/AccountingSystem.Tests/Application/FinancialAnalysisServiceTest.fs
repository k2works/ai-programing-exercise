module AccountingSystem.Tests.Application.FinancialAnalysisServiceTest

open System
open System.Threading.Tasks
open Xunit
open FsUnit.Xunit
open AccountingSystem.Domain.Models
open AccountingSystem.Application.Port.In
open AccountingSystem.Application.Port.Out
open AccountingSystem.Application.Services

/// <summary>
/// テスト用のモック財務諸表リポジトリ
/// </summary>
type MockFinancialStatementRepository() =
    let mutable balanceSheetResult: BalanceSheet option = None
    let mutable incomeStatementResult: IncomeStatement option = None

    member _.SetBalanceSheet(bs: BalanceSheet) = balanceSheetResult <- Some bs
    member _.SetIncomeStatement(is: IncomeStatement) = incomeStatementResult <- Some is

    interface IFinancialStatementRepository with
        member _.GenerateBalanceSheetAsync(asOfDate: DateTime) : Task<BalanceSheet> =
            task {
                match balanceSheetResult with
                | Some bs -> return bs
                | None -> return BalanceSheet.empty asOfDate
            }

        member _.GenerateIncomeStatementAsync(fromDate: DateTime, toDate: DateTime) : Task<IncomeStatement> =
            task {
                match incomeStatementResult with
                | Some is -> return is
                | None -> return IncomeStatement.empty fromDate toDate
            }

/// <summary>
/// FinancialAnalysisService のテスト
/// </summary>
type FinancialAnalysisServiceTest() =

    let createMockRepository () =
        let repo = MockFinancialStatementRepository()

        // D社の令和3年度データをセット
        let balanceSheet = {
            AsOfDate = DateTime(2022, 3, 31)
            Assets = [
                { AccountCode = "111"; AccountName = "現金預金"; Balance = 593256m; Percentage = 0m }
                { AccountCode = "112"; AccountName = "売掛金"; Balance = 1085840m; Percentage = 0m }
                { AccountCode = "114"; AccountName = "棚卸資産"; Balance = 948537m; Percentage = 0m }
                { AccountCode = "115"; AccountName = "その他流動資産"; Balance = 48560m; Percentage = 0m }
                { AccountCode = "1211"; AccountName = "建物"; Balance = 64524m; Percentage = 0m }
                { AccountCode = "122"; AccountName = "無形固定資産"; Balance = 37492m; Percentage = 0m }
                { AccountCode = "123"; AccountName = "投資その他の資産"; Balance = 84957m; Percentage = 0m }
            ]
            Liabilities = [
                { AccountCode = "211"; AccountName = "買掛金"; Balance = 191034m; Percentage = 0m }
                { AccountCode = "212"; AccountName = "短期借入金"; Balance = 120000m; Percentage = 0m }
                { AccountCode = "213"; AccountName = "未払金"; Balance = 197262m; Percentage = 0m }
                { AccountCode = "214"; AccountName = "未払法人税等"; Balance = 250114m; Percentage = 0m }
                { AccountCode = "215"; AccountName = "その他流動負債"; Balance = 92984m; Percentage = 0m }
                { AccountCode = "221"; AccountName = "長期借入金"; Balance = 22500m; Percentage = 0m }
            ]
            Equity = [
                { AccountCode = "31"; AccountName = "資本金"; Balance = 100000m; Percentage = 0m }
                { AccountCode = "33"; AccountName = "利益剰余金"; Balance = 1889272m; Percentage = 0m }
            ]
            TotalAssets = 2863166m
            TotalLiabilities = 873894m
            TotalEquity = 1989272m
            TotalLiabilitiesAndEquity = 2863166m
        }

        let incomeStatement = {
            FromDate = DateTime(2021, 4, 1)
            ToDate = DateTime(2022, 3, 31)
            Revenues = [
                { AccountCode = "41"; AccountName = "売上高"; Balance = 5796105m; Percentage = 100m }
                { AccountCode = "42"; AccountName = "営業外収益"; Balance = 368m; Percentage = 0m }
            ]
            Expenses = [
                { AccountCode = "51"; AccountName = "売上原価"; Balance = 2185856m; Percentage = 0m }
                { AccountCode = "52"; AccountName = "販管費"; Balance = 2625222m; Percentage = 0m }
                { AccountCode = "53"; AccountName = "営業外費用"; Balance = 2676m; Percentage = 0m }
                { AccountCode = "55"; AccountName = "法人税等"; Balance = 331059m; Percentage = 0m }
            ]
            GrossProfit = 3610249m
            OperatingIncome = 985027m
            NetIncome = 651660m
            TotalRevenues = 5796473m
            TotalExpenses = 5144813m
        }

        repo.SetBalanceSheet(balanceSheet)
        repo.SetIncomeStatement(incomeStatement)
        repo

    [<Fact>]
    let ``財務データを取得できる`` () : Task =
        task {
            // Arrange
            let repo = createMockRepository ()
            let service = FinancialAnalysisService(repo) :> IFinancialAnalysisUseCase

            // Act
            let! result = service.GetFinancialDataAsync(2021)

            // Assert
            match result with
            | Ok data ->
                data.FiscalYear |> should equal 2022  // 期末日の年
                data.NetSales |> should equal 5796105m
                data.GrossProfit |> should equal 3610249m
            | Error msg ->
                failwith msg
        }

    [<Fact>]
    let ``包括的な財務指標を計算できる`` () : Task =
        task {
            // Arrange
            let repo = createMockRepository ()
            let service = FinancialAnalysisService(repo) :> IFinancialAnalysisUseCase

            // Act
            let! result = service.CalculateComprehensiveRatiosAsync(2021)

            // Assert
            match result with
            | Ok ratios ->
                ratios.GrossProfitMargin |> should be (greaterThan 60m)
                ratios.OperatingProfitMargin |> should be (greaterThan 15m)
            | Error msg ->
                failwith msg
        }

    [<Fact>]
    let ``収益性分析を実行できる`` () : Task =
        task {
            // Arrange
            let repo = createMockRepository ()
            let service = FinancialAnalysisService(repo) :> IFinancialAnalysisUseCase

            // Act
            let! result = service.AnalyzeProfitabilityAsync(2021)

            // Assert
            match result with
            | Ok analysis ->
                analysis.FiscalYear |> should equal 2021
                analysis.GrossProfitMargin |> should be (greaterThan 60m)
                analysis.Analysis |> should not' (be NullOrEmptyString)
            | Error msg ->
                failwith msg
        }

    [<Fact>]
    let ``効率性分析を実行できる`` () : Task =
        task {
            // Arrange
            let repo = createMockRepository ()
            let service = FinancialAnalysisService(repo) :> IFinancialAnalysisUseCase

            // Act
            let! result = service.AnalyzeEfficiencyAsync(2021)

            // Assert
            match result with
            | Ok analysis ->
                analysis.FiscalYear |> should equal 2021
                analysis.TotalAssetTurnover |> should be (greaterThan 0m)
                analysis.Analysis |> should not' (be NullOrEmptyString)
            | Error msg ->
                failwith msg
        }

    [<Fact>]
    let ``安全性分析を実行できる`` () : Task =
        task {
            // Arrange
            let repo = createMockRepository ()
            let service = FinancialAnalysisService(repo) :> IFinancialAnalysisUseCase

            // Act
            let! result = service.AnalyzeSafetyAsync(2021)

            // Assert
            match result with
            | Ok analysis ->
                analysis.FiscalYear |> should equal 2021
                analysis.CurrentRatio |> should be (greaterThan 100m)
                analysis.EquityRatio |> should be (greaterThan 50m)
                analysis.Analysis |> should not' (be NullOrEmptyString)
            | Error msg ->
                failwith msg
        }

    [<Fact>]
    let ``データがない年度を指定するとエラーが返る`` () : Task =
        task {
            // Arrange
            let repo = MockFinancialStatementRepository()
            let service = FinancialAnalysisService(repo) :> IFinancialAnalysisUseCase

            // Act
            let! result = service.GetFinancialDataAsync(2025)

            // Assert
            match result with
            | Ok _ ->
                failwith "エラーが期待されました"
            | Error msg ->
                msg |> should haveSubstring "データが見つかりません"
        }
