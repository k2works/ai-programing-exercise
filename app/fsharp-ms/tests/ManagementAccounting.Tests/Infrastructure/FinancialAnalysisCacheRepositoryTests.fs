namespace ManagementAccounting.Tests.Infrastructure

open System
open Xunit
open ManagementAccounting.Domain.Models
open ManagementAccounting.Application.Ports.Out
open ManagementAccounting.Infrastructure.Persistence.Repositories

/// <summary>
/// 財務分析キャッシュリポジトリのテスト（Testcontainers 使用）
/// </summary>
[<Collection("PostgresCollection")>]
type FinancialAnalysisCacheRepositoryTests(fixture: PostgresTestFixture) =

    let createTestData () : FinancialData =
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

    let createTestRatios () : FinancialRatios =
        {
            OperatingProfitMargin = 0.1m
            TotalAssetTurnover = 2m
            FixedAssetTurnover = 5m
            CurrentRatio = 2m
            QuickRatio = 1.333m
            EquityRatio = 0.5m
            ReturnOnAssets = 0.2m
            ReturnOnEquity = 0.4m
        }

    [<Fact>]
    member _.``キャッシュを保存できる``() =
        task {
            // Arrange
            let repository = FinancialAnalysisCacheRepository(fixture.ConnectionString) :> IFinancialAnalysisCacheRepository
            let cache = {
                Id = None
                FiscalYear = 2030  // ユニークな年度
                Data = createTestData()
                Ratios = createTestRatios()
                CachedAt = DateTime.UtcNow
            }

            // Act
            let! saved = repository.SaveAsync(cache)

            // Assert
            Assert.True(saved.Id.IsSome)
            Assert.True(saved.Id.Value > 0)
        }

    [<Fact>]
    member _.``会計年度でキャッシュを取得できる``() =
        task {
            // Arrange
            let repository = FinancialAnalysisCacheRepository(fixture.ConnectionString) :> IFinancialAnalysisCacheRepository
            let cache = {
                Id = None
                FiscalYear = 2031  // ユニークな年度
                Data = createTestData()
                Ratios = createTestRatios()
                CachedAt = DateTime.UtcNow
            }
            let! _ = repository.SaveAsync(cache)

            // Act
            let! retrieved = repository.GetByFiscalYearAsync(2031)

            // Assert
            Assert.True(retrieved.IsSome)
            Assert.Equal(2031, retrieved.Value.FiscalYear)
            Assert.Equal(1000000m, retrieved.Value.Data.Sales)
        }

    [<Fact>]
    member _.``存在しない会計年度はNoneを返す``() =
        task {
            // Arrange
            let repository = FinancialAnalysisCacheRepository(fixture.ConnectionString) :> IFinancialAnalysisCacheRepository

            // Act
            let! retrieved = repository.GetByFiscalYearAsync(9999)

            // Assert
            Assert.True(retrieved.IsNone)
        }

    [<Fact>]
    member _.``キャッシュを削除できる``() =
        task {
            // Arrange
            let repository = FinancialAnalysisCacheRepository(fixture.ConnectionString) :> IFinancialAnalysisCacheRepository
            let cache = {
                Id = None
                FiscalYear = 2032  // ユニークな年度
                Data = createTestData()
                Ratios = createTestRatios()
                CachedAt = DateTime.UtcNow
            }
            let! _ = repository.SaveAsync(cache)

            // Act
            let! deleted = repository.DeleteByFiscalYearAsync(2032)

            // Assert
            Assert.True(deleted)

            let! retrieved = repository.GetByFiscalYearAsync(2032)
            Assert.True(retrieved.IsNone)
        }

    [<Fact>]
    member _.``存在しないキャッシュの削除はfalseを返す``() =
        task {
            // Arrange
            let repository = FinancialAnalysisCacheRepository(fixture.ConnectionString) :> IFinancialAnalysisCacheRepository

            // Act
            let! deleted = repository.DeleteByFiscalYearAsync(9998)

            // Assert
            Assert.False(deleted)
        }

    [<Fact>]
    member _.``同じ会計年度で保存すると更新される``() =
        task {
            // Arrange
            let repository = FinancialAnalysisCacheRepository(fixture.ConnectionString) :> IFinancialAnalysisCacheRepository
            let originalCache = {
                Id = None
                FiscalYear = 2033  // ユニークな年度
                Data = createTestData()
                Ratios = createTestRatios()
                CachedAt = DateTime.UtcNow
            }
            let! _ = repository.SaveAsync(originalCache)

            // 更新されたデータで保存
            let updatedData = { createTestData() with Sales = 2000000m }
            let updatedCache = {
                Id = None
                FiscalYear = 2033
                Data = updatedData
                Ratios = createTestRatios()
                CachedAt = DateTime.UtcNow
            }

            // Act
            let! _ = repository.SaveAsync(updatedCache)

            // Assert
            let! retrieved = repository.GetByFiscalYearAsync(2033)
            Assert.True(retrieved.IsSome)
            Assert.Equal(2000000m, retrieved.Value.Data.Sales)
        }
