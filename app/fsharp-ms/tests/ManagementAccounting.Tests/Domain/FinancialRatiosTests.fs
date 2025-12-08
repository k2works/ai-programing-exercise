namespace ManagementAccounting.Tests.Domain

open Xunit
open ManagementAccounting.Domain.Models

module FinancialRatiosTests =

    let createTestFinancialData () : FinancialData =
        {
            FiscalYear = 2024
            Sales = 1000000m
            OperatingProfit = 100000m
            TotalAssets = 500000m
            TangibleFixedAssets = 200000m
            CurrentAssets = 300000m
            CurrentLiabilities = 150000m
            QuickAssets = 200000m
            Equity = 250000m
        }

    [<Fact>]
    let ``売上高営業利益率を正しく計算できる`` () =
        // Arrange
        let data = createTestFinancialData()

        // Act
        let ratios = FinancialRatios.calculate data

        // Assert
        Assert.Equal(0.1m, ratios.OperatingProfitMargin) // 100000 / 1000000 = 0.1

    [<Fact>]
    let ``総資産回転率を正しく計算できる`` () =
        // Arrange
        let data = createTestFinancialData()

        // Act
        let ratios = FinancialRatios.calculate data

        // Assert
        Assert.Equal(2m, ratios.TotalAssetTurnover) // 1000000 / 500000 = 2

    [<Fact>]
    let ``固定資産回転率を正しく計算できる`` () =
        // Arrange
        let data = createTestFinancialData()

        // Act
        let ratios = FinancialRatios.calculate data

        // Assert
        Assert.Equal(5m, ratios.FixedAssetTurnover) // 1000000 / 200000 = 5

    [<Fact>]
    let ``流動比率を正しく計算できる`` () =
        // Arrange
        let data = createTestFinancialData()

        // Act
        let ratios = FinancialRatios.calculate data

        // Assert
        Assert.Equal(2m, ratios.CurrentRatio) // 300000 / 150000 = 2

    [<Fact>]
    let ``当座比率を正しく計算できる`` () =
        // Arrange
        let data = createTestFinancialData()

        // Act
        let ratios = FinancialRatios.calculate data

        // Assert
        Assert.True(ratios.QuickRatio > 1.33m && ratios.QuickRatio < 1.34m) // 200000 / 150000 = 1.333...

    [<Fact>]
    let ``自己資本比率を正しく計算できる`` () =
        // Arrange
        let data = createTestFinancialData()

        // Act
        let ratios = FinancialRatios.calculate data

        // Assert
        Assert.Equal(0.5m, ratios.EquityRatio) // 250000 / 500000 = 0.5

    [<Fact>]
    let ``ROAを正しく計算できる`` () =
        // Arrange
        let data = createTestFinancialData()

        // Act
        let ratios = FinancialRatios.calculate data

        // Assert
        Assert.Equal(0.2m, ratios.ReturnOnAssets) // 100000 / 500000 = 0.2

    [<Fact>]
    let ``ROEを正しく計算できる`` () =
        // Arrange
        let data = createTestFinancialData()

        // Act
        let ratios = FinancialRatios.calculate data

        // Assert
        Assert.Equal(0.4m, ratios.ReturnOnEquity) // 100000 / 250000 = 0.4

    [<Fact>]
    let ``分母が0の場合は0を返す`` () =
        // Arrange
        let data = {
            FiscalYear = 2024
            Sales = 0m
            OperatingProfit = 100000m
            TotalAssets = 0m
            TangibleFixedAssets = 0m
            CurrentAssets = 300000m
            CurrentLiabilities = 0m
            QuickAssets = 200000m
            Equity = 0m
        }

        // Act
        let ratios = FinancialRatios.calculate data

        // Assert
        Assert.Equal(0m, ratios.OperatingProfitMargin)
        Assert.Equal(0m, ratios.TotalAssetTurnover)
        Assert.Equal(0m, ratios.FixedAssetTurnover)
        Assert.Equal(0m, ratios.CurrentRatio)
        Assert.Equal(0m, ratios.QuickRatio)
        Assert.Equal(0m, ratios.EquityRatio)
        Assert.Equal(0m, ratios.ReturnOnAssets)
        Assert.Equal(0m, ratios.ReturnOnEquity)
