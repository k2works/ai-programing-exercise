namespace ManagementAccounting.Tests.Application

open System
open System.Threading.Tasks
open Xunit
open Microsoft.Extensions.Logging
open Shared.Contracts.Events
open ManagementAccounting.Domain.Models
open ManagementAccounting.Application.Ports.In
open ManagementAccounting.Application.Ports.Out
open ManagementAccounting.Application.UseCases

/// <summary>
/// モックロガー
/// </summary>
type MockLogger<'T>() =
    interface ILogger<'T> with
        member _.BeginScope(_: 'TState) : IDisposable = null
        member _.IsEnabled(_: LogLevel) = true
        member _.Log(_: LogLevel, _: EventId, _: 'TState, _: exn, _: Func<'TState, exn, string>) = ()

module JournalEventHandlerTests =

    let createTestCache (fiscalYear: int) : FinancialAnalysisCache =
        {
            Id = Some 1
            FiscalYear = fiscalYear
            Data = {
                FiscalYear = fiscalYear
                Sales = 1000000m
                OperatingProfit = 100000m
                TotalAssets = 500000m
                TangibleFixedAssets = 200000m
                CurrentAssets = 300000m
                CurrentLiabilities = 150000m
                QuickAssets = 200000m
                Equity = 250000m
            }
            Ratios = {
                OperatingProfitMargin = 0.1m
                TotalAssetTurnover = 2.0m
                FixedAssetTurnover = 5.0m
                CurrentRatio = 2.0m
                QuickRatio = 1.33m
                EquityRatio = 0.5m
                ReturnOnAssets = 0.2m
                ReturnOnEquity = 0.4m
            }
            CachedAt = DateTime.UtcNow
        }

    [<Fact>]
    let ``仕訳作成イベントでキャッシュが再構築される`` () =
        task {
            // Arrange
            let cacheRepo = MockCacheRepository()
            let financialDataPort = MockFinancialDataPort()
            let logger = MockLogger<JournalEventHandler>()
            let handler = JournalEventHandler(cacheRepo, financialDataPort, logger) :> IJournalEventHandler

            // キャッシュを事前に作成
            let! _ = (cacheRepo :> IFinancialAnalysisCacheRepository).SaveAsync(createTestCache 2024)

            let event: JournalCreatedEvent = {
                JournalId = 1
                FiscalYear = 2024
                JournalDate = DateTime.UtcNow
                Description = "テスト仕訳"
                TotalAmount = 100000m
                CreatedAt = DateTime.UtcNow
            }

            // Act
            do! handler.HandleJournalCreatedAsync(event)

            // Assert - キャッシュが再構築されていること
            let! cached = (cacheRepo :> IFinancialAnalysisCacheRepository).GetByFiscalYearAsync(2024)
            Assert.True(cached.IsSome)
        }

    [<Fact>]
    let ``仕訳更新イベントでキャッシュが再構築される`` () =
        task {
            // Arrange
            let cacheRepo = MockCacheRepository()
            let financialDataPort = MockFinancialDataPort()
            let logger = MockLogger<JournalEventHandler>()
            let handler = JournalEventHandler(cacheRepo, financialDataPort, logger) :> IJournalEventHandler

            // キャッシュを事前に作成
            let! _ = (cacheRepo :> IFinancialAnalysisCacheRepository).SaveAsync(createTestCache 2024)

            let event: JournalUpdatedEvent = {
                JournalId = 1
                FiscalYear = 2024
                JournalDate = DateTime.UtcNow
                Description = "更新された仕訳"
                Entries = [
                    { AccountCode = "1110"; DebitAmount = 50000m; CreditAmount = 0m; Description = "現金" }
                    { AccountCode = "4110"; DebitAmount = 0m; CreditAmount = 50000m; Description = "売上" }
                ]
                UpdatedAt = DateTime.UtcNow
            }

            // Act
            do! handler.HandleJournalUpdatedAsync(event)

            // Assert - キャッシュが再構築されていること
            let! cached = (cacheRepo :> IFinancialAnalysisCacheRepository).GetByFiscalYearAsync(2024)
            Assert.True(cached.IsSome)
        }

    [<Fact>]
    let ``仕訳削除イベントでキャッシュが再構築される`` () =
        task {
            // Arrange
            let cacheRepo = MockCacheRepository()
            let financialDataPort = MockFinancialDataPort()
            let logger = MockLogger<JournalEventHandler>()
            let handler = JournalEventHandler(cacheRepo, financialDataPort, logger) :> IJournalEventHandler

            // キャッシュを事前に作成
            let! _ = (cacheRepo :> IFinancialAnalysisCacheRepository).SaveAsync(createTestCache 2024)

            let event: JournalDeletedEvent = {
                JournalId = 1
                FiscalYear = 2024
                DeletedAt = DateTime.UtcNow
            }

            // Act
            do! handler.HandleJournalDeletedAsync(event)

            // Assert - キャッシュが再構築されていること
            let! cached = (cacheRepo :> IFinancialAnalysisCacheRepository).GetByFiscalYearAsync(2024)
            Assert.True(cached.IsSome)
        }

    [<Fact>]
    let ``キャッシュがない場合でもキャッシュが構築される`` () =
        task {
            // Arrange
            let cacheRepo = MockCacheRepository()
            let financialDataPort = MockFinancialDataPort()
            let logger = MockLogger<JournalEventHandler>()
            let handler = JournalEventHandler(cacheRepo, financialDataPort, logger) :> IJournalEventHandler

            // キャッシュなし

            let event: JournalCreatedEvent = {
                JournalId = 1
                FiscalYear = 2024
                JournalDate = DateTime.UtcNow
                Description = "テスト仕訳"
                TotalAmount = 100000m
                CreatedAt = DateTime.UtcNow
            }

            // Act
            do! handler.HandleJournalCreatedAsync(event)

            // Assert - キャッシュが構築されていること
            let! cached = (cacheRepo :> IFinancialAnalysisCacheRepository).GetByFiscalYearAsync(2024)
            Assert.True(cached.IsSome)
        }

    [<Fact>]
    let ``異なる会計年度のキャッシュは影響を受けない`` () =
        task {
            // Arrange
            let cacheRepo = MockCacheRepository()
            let financialDataPort = MockFinancialDataPort()
            let logger = MockLogger<JournalEventHandler>()
            let handler = JournalEventHandler(cacheRepo, financialDataPort, logger) :> IJournalEventHandler

            // 複数の会計年度のキャッシュを作成
            let! _ = (cacheRepo :> IFinancialAnalysisCacheRepository).SaveAsync(createTestCache 2023)
            let! _ = (cacheRepo :> IFinancialAnalysisCacheRepository).SaveAsync(createTestCache 2024)

            let event: JournalCreatedEvent = {
                JournalId = 1
                FiscalYear = 2024
                JournalDate = DateTime.UtcNow
                Description = "テスト仕訳"
                TotalAmount = 100000m
                CreatedAt = DateTime.UtcNow
            }

            // Act
            do! handler.HandleJournalCreatedAsync(event)

            // Assert
            let! cache2023 = (cacheRepo :> IFinancialAnalysisCacheRepository).GetByFiscalYearAsync(2023)
            let! cache2024 = (cacheRepo :> IFinancialAnalysisCacheRepository).GetByFiscalYearAsync(2024)

            Assert.True(cache2023.IsSome) // 2023 は残っている
            Assert.True(cache2024.IsSome) // 2024 は再構築されている
        }
