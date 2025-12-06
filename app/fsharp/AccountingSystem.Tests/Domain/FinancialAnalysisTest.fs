module AccountingSystem.Tests.Domain.FinancialAnalysisTest

open System
open Xunit
open FsUnit.Xunit
open AccountingSystem.Domain.Models

/// <summary>
/// FinancialData モジュールのテスト
/// </summary>
module FinancialDataTests =

    [<Fact>]
    let ``空の財務データを作成できる`` () =
        // Arrange & Act
        let data = FinancialData.empty 2021 (DateTime(2022, 3, 31))

        // Assert
        data.FiscalYear |> should equal 2021
        data.TotalAssets |> should equal 0m
        data.NetSales |> should equal 0m

    [<Fact>]
    let ``貸借対照表と損益計算書から財務データを構築できる`` () =
        // Arrange
        let balanceSheet = {
            AsOfDate = DateTime(2022, 3, 31)
            Assets = [
                { AccountCode = "111"; AccountName = "現金預金"; Balance = 593256m; Percentage = 0m }
                { AccountCode = "112"; AccountName = "売掛金"; Balance = 1085840m; Percentage = 0m }
                { AccountCode = "114"; AccountName = "棚卸資産"; Balance = 948537m; Percentage = 0m }
                { AccountCode = "1211"; AccountName = "建物"; Balance = 64524m; Percentage = 0m }
            ]
            Liabilities = [
                { AccountCode = "211"; AccountName = "買掛金"; Balance = 191034m; Percentage = 0m }
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
            ]
            GrossProfit = 3610249m
            OperatingIncome = 985027m
            NetIncome = 651660m
            TotalRevenues = 5796473m
            TotalExpenses = 4813754m
        }

        // Act
        let data = FinancialData.fromStatements balanceSheet incomeStatement

        // Assert
        data.FiscalYear |> should equal 2022
        data.NetSales |> should equal 5796105m
        data.CostOfSales |> should equal 2185856m
        data.GrossProfit |> should equal 3610249m

/// <summary>
/// ComprehensiveFinancialRatios モジュールのテスト
/// </summary>
module ComprehensiveFinancialRatiosTests =

    [<Fact>]
    let ``空の財務指標を作成できる`` () =
        // Arrange & Act
        let ratios = ComprehensiveFinancialRatios.empty 2021

        // Assert
        ratios.FiscalYear |> should equal 2021
        ratios.GrossProfitMargin |> should equal 0m
        ratios.CurrentRatio |> should equal 0m

    [<Fact>]
    let ``売上高総利益率を正しく計算できる`` () =
        // Arrange
        let data = {
            FinancialData.empty 2021 (DateTime(2022, 3, 31)) with
                NetSales = 5796105m
                GrossProfit = 3610249m
        }

        // Act
        let ratios = ComprehensiveFinancialRatios.calculate data

        // Assert
        // 3610249 / 5796105 * 100 = 62.29%
        ratios.GrossProfitMargin |> should be (greaterThan 62m)
        ratios.GrossProfitMargin |> should be (lessThan 63m)

    [<Fact>]
    let ``売上高営業利益率を正しく計算できる`` () =
        // Arrange
        let data = {
            FinancialData.empty 2021 (DateTime(2022, 3, 31)) with
                NetSales = 5796105m
                OperatingIncome = 985027m
        }

        // Act
        let ratios = ComprehensiveFinancialRatios.calculate data

        // Assert
        // 985027 / 5796105 * 100 = 16.99%
        ratios.OperatingProfitMargin |> should be (greaterThan 16m)
        ratios.OperatingProfitMargin |> should be (lessThan 18m)

    [<Fact>]
    let ``流動比率を正しく計算できる`` () =
        // Arrange
        let data = {
            FinancialData.empty 2021 (DateTime(2022, 3, 31)) with
                CurrentAssets = 2676193m
                CurrentLiabilities = 851394m
        }

        // Act
        let ratios = ComprehensiveFinancialRatios.calculate data

        // Assert
        // 2676193 / 851394 * 100 = 314.33%
        ratios.CurrentRatio |> should be (greaterThan 314m)
        ratios.CurrentRatio |> should be (lessThan 315m)

    [<Fact>]
    let ``自己資本比率を正しく計算できる`` () =
        // Arrange
        let data = {
            FinancialData.empty 2021 (DateTime(2022, 3, 31)) with
                Equity = 1989272m
                TotalAssets = 2863166m
        }

        // Act
        let ratios = ComprehensiveFinancialRatios.calculate data

        // Assert
        // 1989272 / 2863166 * 100 = 69.48%
        ratios.EquityRatio |> should be (greaterThan 69m)
        ratios.EquityRatio |> should be (lessThan 70m)

    [<Fact>]
    let ``総資本回転率を正しく計算できる`` () =
        // Arrange
        let data = {
            FinancialData.empty 2021 (DateTime(2022, 3, 31)) with
                NetSales = 5796105m
                TotalAssets = 2863166m
        }

        // Act
        let ratios = ComprehensiveFinancialRatios.calculate data

        // Assert
        // 5796105 / 2863166 = 2.02 回
        ratios.TotalAssetTurnover |> should be (greaterThan 2.0m)
        ratios.TotalAssetTurnover |> should be (lessThan 2.1m)

    [<Fact>]
    let ``ゼロ除算を回避できる`` () =
        // Arrange
        let data = FinancialData.empty 2021 (DateTime(2022, 3, 31))

        // Act
        let ratios = ComprehensiveFinancialRatios.calculate data

        // Assert - すべてゼロで例外なし
        ratios.GrossProfitMargin |> should equal 0m
        ratios.CurrentRatio |> should equal 0m
        ratios.TotalAssetTurnover |> should equal 0m
