module AccountingSystem.Tests.Domain.FinancialRatiosTest

open System
open Xunit
open FsUnit.Xunit
open AccountingSystem.Domain.Models

/// <summary>
/// FinancialRatios（財務指標）のテスト
/// </summary>
type FinancialRatiosTest() =

    /// テスト用の貸借対照表を作成
    member private _.CreateBalanceSheet
        (currentAssets: decimal)
        (fixedAssets: decimal)
        (currentLiabilities: decimal)
        (longTermLiabilities: decimal)
        (equity: decimal) : BalanceSheet =
        {
            AsOfDate = DateTime(2024, 1, 31)
            Assets = [
                { AccountCode = "1110"; AccountName = "普通預金"; Balance = currentAssets; Percentage = 0M }
                { AccountCode = "1410"; AccountName = "建物"; Balance = fixedAssets; Percentage = 0M }
            ]
            Liabilities = [
                { AccountCode = "2110"; AccountName = "買掛金"; Balance = currentLiabilities; Percentage = 0M }
                { AccountCode = "2510"; AccountName = "長期借入金"; Balance = longTermLiabilities; Percentage = 0M }
            ]
            Equity = [
                { AccountCode = "3110"; AccountName = "資本金"; Balance = equity; Percentage = 0M }
            ]
            TotalAssets = currentAssets + fixedAssets
            TotalLiabilities = currentLiabilities + longTermLiabilities
            TotalEquity = equity
            TotalLiabilitiesAndEquity = currentLiabilities + longTermLiabilities + equity
        }

    /// テスト用の損益計算書を作成
    member private _.CreateIncomeStatement
        (revenues: decimal)
        (costOfSales: decimal)
        (operatingExpenses: decimal) : IncomeStatement =
        let totalExpenses = costOfSales + operatingExpenses
        let grossProfit = revenues - costOfSales
        let operatingIncome = grossProfit - operatingExpenses
        let netIncome = revenues - totalExpenses
        {
            FromDate = DateTime(2024, 1, 1)
            ToDate = DateTime(2024, 1, 31)
            Revenues = [
                { AccountCode = "4110"; AccountName = "売上高"; Balance = revenues; Percentage = 100M }
            ]
            Expenses = [
                { AccountCode = "5110"; AccountName = "売上原価"; Balance = costOfSales; Percentage = 0M }
                { AccountCode = "6110"; AccountName = "販売管理費"; Balance = operatingExpenses; Percentage = 0M }
            ]
            GrossProfit = grossProfit
            OperatingIncome = operatingIncome
            NetIncome = netIncome
            TotalRevenues = revenues
            TotalExpenses = totalExpenses
        }

    [<Fact>]
    member this.``流動比率を計算できる``() =
        // Given: 流動資産8,000,000、流動負債500,000
        let balanceSheet = this.CreateBalanceSheet 8000000M 2000000M 500000M 4500000M 5000000M
        let incomeStatement = this.CreateIncomeStatement 10000000M 6000000M 2000000M

        // When: 財務指標を計算
        let ratios = FinancialRatios.calculate balanceSheet incomeStatement

        // Then: 流動比率 = 8,000,000 / 500,000 × 100 = 1600%
        ratios.CurrentRatio |> should equal 1600.00M

    [<Fact>]
    member this.``自己資本比率を計算できる``() =
        // Given: 総資産10,000,000、純資産5,000,000
        let balanceSheet = this.CreateBalanceSheet 8000000M 2000000M 500000M 4500000M 5000000M
        let incomeStatement = this.CreateIncomeStatement 10000000M 6000000M 2000000M

        // When: 財務指標を計算
        let ratios = FinancialRatios.calculate balanceSheet incomeStatement

        // Then: 自己資本比率 = 5,000,000 / 10,000,000 × 100 = 50%
        ratios.EquityRatio |> should equal 50.00M

    [<Fact>]
    member this.``売上総利益率を計算できる``() =
        // Given: 売上高10,000,000、売上総利益4,000,000
        let balanceSheet = this.CreateBalanceSheet 8000000M 2000000M 500000M 4500000M 5000000M
        let incomeStatement = this.CreateIncomeStatement 10000000M 6000000M 2000000M

        // When: 財務指標を計算
        let ratios = FinancialRatios.calculate balanceSheet incomeStatement

        // Then: 売上総利益率 = 4,000,000 / 10,000,000 × 100 = 40%
        ratios.GrossProfitMargin |> should equal 40.00M

    [<Fact>]
    member this.``営業利益率を計算できる``() =
        // Given: 売上高10,000,000、営業利益2,000,000
        let balanceSheet = this.CreateBalanceSheet 8000000M 2000000M 500000M 4500000M 5000000M
        let incomeStatement = this.CreateIncomeStatement 10000000M 6000000M 2000000M

        // When: 財務指標を計算
        let ratios = FinancialRatios.calculate balanceSheet incomeStatement

        // Then: 営業利益率 = 2,000,000 / 10,000,000 × 100 = 20%
        ratios.OperatingProfitMargin |> should equal 20.00M

    [<Fact>]
    member this.``当期純利益率を計算できる``() =
        // Given: 売上高10,000,000、当期純利益2,000,000
        let balanceSheet = this.CreateBalanceSheet 8000000M 2000000M 500000M 4500000M 5000000M
        let incomeStatement = this.CreateIncomeStatement 10000000M 6000000M 2000000M

        // When: 財務指標を計算
        let ratios = FinancialRatios.calculate balanceSheet incomeStatement

        // Then: 当期純利益率 = 2,000,000 / 10,000,000 × 100 = 20%
        ratios.NetProfitMargin |> should equal 20.00M

    [<Fact>]
    member this.``ROAを計算できる``() =
        // Given: 総資産10,000,000、当期純利益2,000,000
        let balanceSheet = this.CreateBalanceSheet 8000000M 2000000M 500000M 4500000M 5000000M
        let incomeStatement = this.CreateIncomeStatement 10000000M 6000000M 2000000M

        // When: 財務指標を計算
        let ratios = FinancialRatios.calculate balanceSheet incomeStatement

        // Then: ROA = 2,000,000 / 10,000,000 × 100 = 20%
        ratios.Roa |> should equal 20.00M

    [<Fact>]
    member this.``ROEを計算できる``() =
        // Given: 純資産5,000,000、当期純利益2,000,000
        let balanceSheet = this.CreateBalanceSheet 8000000M 2000000M 500000M 4500000M 5000000M
        let incomeStatement = this.CreateIncomeStatement 10000000M 6000000M 2000000M

        // When: 財務指標を計算
        let ratios = FinancialRatios.calculate balanceSheet incomeStatement

        // Then: ROE = 2,000,000 / 5,000,000 × 100 = 40%
        ratios.Roe |> should equal 40.00M

    [<Fact>]
    member this.``ゼロ除算の場合は0を返す``() =
        // Given: すべてゼロの貸借対照表・損益計算書
        let balanceSheet = {
            AsOfDate = DateTime(2024, 1, 31)
            Assets = []
            Liabilities = []
            Equity = []
            TotalAssets = 0M
            TotalLiabilities = 0M
            TotalEquity = 0M
            TotalLiabilitiesAndEquity = 0M
        }
        let incomeStatement = {
            FromDate = DateTime(2024, 1, 1)
            ToDate = DateTime(2024, 1, 31)
            Revenues = []
            Expenses = []
            GrossProfit = 0M
            OperatingIncome = 0M
            NetIncome = 0M
            TotalRevenues = 0M
            TotalExpenses = 0M
        }

        // When: 財務指標を計算
        let ratios = FinancialRatios.calculate balanceSheet incomeStatement

        // Then: すべての指標が0
        ratios.CurrentRatio |> should equal 0M
        ratios.EquityRatio |> should equal 0M
        ratios.GrossProfitMargin |> should equal 0M
        ratios.OperatingProfitMargin |> should equal 0M
        ratios.NetProfitMargin |> should equal 0M
        ratios.Roa |> should equal 0M
        ratios.Roe |> should equal 0M

    [<Fact>]
    member _.``空の財務指標を作成できる``() =
        // When: 空の財務指標を作成
        let ratios = FinancialRatios.empty

        // Then: すべての指標が0
        ratios.CurrentRatio |> should equal 0M
        ratios.EquityRatio |> should equal 0M
        ratios.GrossProfitMargin |> should equal 0M
        ratios.OperatingProfitMargin |> should equal 0M
        ratios.NetProfitMargin |> should equal 0M
        ratios.Roa |> should equal 0M
        ratios.Roe |> should equal 0M
