namespace ManagementAccounting.Tests.Application

open System
open System.Threading.Tasks
open Xunit
open ManagementAccounting.Domain.Models
open ManagementAccounting.Application.Ports.In
open ManagementAccounting.Application.Ports.Out
open ManagementAccounting.Application.UseCases

/// <summary>
/// モック財務データポート
/// </summary>
type MockFinancialDataPort() =
    let mutable shouldFail = false
    let mutable testData: FinancialData option = None

    member _.SetTestData(data: FinancialData) =
        testData <- Some data

    member _.SetShouldFail(value: bool) =
        shouldFail <- value

    interface IFinancialDataPort with
        member _.FetchFinancialDataByFiscalYearAsync(fiscalYear: int) =
            task {
                if shouldFail then
                    return raise (Exception("テスト用エラー"))
                else
                    return
                        testData
                        |> Option.defaultValue {
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
            }

/// <summary>
/// モックキャッシュリポジトリ
/// </summary>
type MockCacheRepository() =
    let mutable caches: Map<int, FinancialAnalysisCache> = Map.empty

    member _.GetCaches() = caches

    interface IFinancialAnalysisCacheRepository with
        member _.SaveAsync(cache: FinancialAnalysisCache) =
            task {
                let saved = { cache with Id = Some (caches.Count + 1) }
                caches <- caches |> Map.add cache.FiscalYear saved
                return saved
            }

        member _.GetByFiscalYearAsync(fiscalYear: int) =
            task {
                return caches |> Map.tryFind fiscalYear
            }

        member _.DeleteByFiscalYearAsync(fiscalYear: int) =
            task {
                let exists = caches |> Map.containsKey fiscalYear
                if exists then
                    caches <- caches |> Map.remove fiscalYear
                return exists
            }

module FinancialAnalysisUseCaseTests =

    [<Fact>]
    let ``財務分析を実行できる`` () =
        task {
            // Arrange
            let dataPort = MockFinancialDataPort()
            let cacheRepo = MockCacheRepository()
            let useCase = FinancialAnalysisUseCase(dataPort, cacheRepo) :> IFinancialAnalysisUseCase
            let request = { FiscalYear = 2024; UseCache = false }

            // Act
            let! result = useCase.AnalyzeAsync(request)

            // Assert
            match result with
            | Ok analysis ->
                Assert.Equal(2024, analysis.FiscalYear)
                Assert.Equal(1000000m, analysis.Data.Sales)
                Assert.Equal(0.1m, analysis.Ratios.OperatingProfitMargin)
            | Error msg ->
                Assert.Fail(msg)
        }

    [<Fact>]
    let ``分析結果がキャッシュに保存される`` () =
        task {
            // Arrange
            let dataPort = MockFinancialDataPort()
            let cacheRepo = MockCacheRepository()
            let useCase = FinancialAnalysisUseCase(dataPort, cacheRepo) :> IFinancialAnalysisUseCase
            let request = { FiscalYear = 2024; UseCache = false }

            // Act
            let! _ = useCase.AnalyzeAsync(request)

            // Assert
            let caches = cacheRepo.GetCaches()
            Assert.True(caches |> Map.containsKey 2024)
        }

    [<Fact>]
    let ``キャッシュを使用する場合はキャッシュから返す`` () =
        task {
            // Arrange
            let dataPort = MockFinancialDataPort()
            let cacheRepo = MockCacheRepository()
            let useCase = FinancialAnalysisUseCase(dataPort, cacheRepo) :> IFinancialAnalysisUseCase

            // 最初の分析でキャッシュを作成
            let! _ = useCase.AnalyzeAsync({ FiscalYear = 2024; UseCache = false })

            // データポートのデータを変更
            dataPort.SetTestData({
                FiscalYear = 2024
                Sales = 2000000m // 変更
                OperatingProfit = 200000m
                TotalAssets = 500000m
                TangibleFixedAssets = 200000m
                CurrentAssets = 300000m
                CurrentLiabilities = 150000m
                QuickAssets = 200000m
                Equity = 250000m
            })

            // Act - キャッシュを使用
            let! result = useCase.AnalyzeAsync({ FiscalYear = 2024; UseCache = true })

            // Assert - キャッシュの値（元の値）が返される
            match result with
            | Ok analysis ->
                Assert.Equal(1000000m, analysis.Data.Sales) // キャッシュの値
            | Error msg ->
                Assert.Fail(msg)
        }

    [<Fact>]
    let ``キャッシュがない場合はサービスから取得`` () =
        task {
            // Arrange
            let dataPort = MockFinancialDataPort()
            let cacheRepo = MockCacheRepository()
            let useCase = FinancialAnalysisUseCase(dataPort, cacheRepo) :> IFinancialAnalysisUseCase

            // Act - キャッシュを使用するがキャッシュは空
            let! result = useCase.AnalyzeAsync({ FiscalYear = 2024; UseCache = true })

            // Assert
            match result with
            | Ok analysis ->
                Assert.Equal(2024, analysis.FiscalYear)
            | Error msg ->
                Assert.Fail(msg)
        }

    [<Fact>]
    let ``キャッシュされた分析結果を取得できる`` () =
        task {
            // Arrange
            let dataPort = MockFinancialDataPort()
            let cacheRepo = MockCacheRepository()
            let useCase = FinancialAnalysisUseCase(dataPort, cacheRepo) :> IFinancialAnalysisUseCase

            // キャッシュを作成
            let! _ = useCase.AnalyzeAsync({ FiscalYear = 2024; UseCache = false })

            // Act
            let! cached = useCase.GetCachedAnalysisAsync(2024)

            // Assert
            Assert.True(cached.IsSome)
            Assert.Equal(2024, cached.Value.FiscalYear)
        }

    [<Fact>]
    let ``キャッシュがない場合はNoneを返す`` () =
        task {
            // Arrange
            let dataPort = MockFinancialDataPort()
            let cacheRepo = MockCacheRepository()
            let useCase = FinancialAnalysisUseCase(dataPort, cacheRepo) :> IFinancialAnalysisUseCase

            // Act
            let! cached = useCase.GetCachedAnalysisAsync(2024)

            // Assert
            Assert.True(cached.IsNone)
        }

    [<Fact>]
    let ``キャッシュを無効化できる`` () =
        task {
            // Arrange
            let dataPort = MockFinancialDataPort()
            let cacheRepo = MockCacheRepository()
            let useCase = FinancialAnalysisUseCase(dataPort, cacheRepo) :> IFinancialAnalysisUseCase

            // キャッシュを作成
            let! _ = useCase.AnalyzeAsync({ FiscalYear = 2024; UseCache = false })

            // Act
            let! deleted = useCase.InvalidateCacheAsync(2024)

            // Assert
            Assert.True(deleted)

            let! cached = useCase.GetCachedAnalysisAsync(2024)
            Assert.True(cached.IsNone)
        }

    [<Fact>]
    let ``エラー発生時はErrorを返す`` () =
        task {
            // Arrange
            let dataPort = MockFinancialDataPort()
            dataPort.SetShouldFail(true)
            let cacheRepo = MockCacheRepository()
            let useCase = FinancialAnalysisUseCase(dataPort, cacheRepo) :> IFinancialAnalysisUseCase
            let request = { FiscalYear = 2024; UseCache = false }

            // Act
            let! result = useCase.AnalyzeAsync(request)

            // Assert
            match result with
            | Ok _ -> Assert.Fail("エラーが期待されます")
            | Error msg -> Assert.Contains("財務分析に失敗しました", msg)
        }
