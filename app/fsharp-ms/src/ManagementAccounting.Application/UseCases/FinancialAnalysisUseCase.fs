namespace ManagementAccounting.Application.UseCases

open System
open System.Threading.Tasks
open ManagementAccounting.Domain.Models
open ManagementAccounting.Application.Ports.In
open ManagementAccounting.Application.Ports.Out

/// <summary>
/// 財務分析ユースケース実装
/// </summary>
type FinancialAnalysisUseCase(
    financialDataPort: IFinancialDataPort,
    cacheRepository: IFinancialAnalysisCacheRepository) =

    let analyzeFromService (fiscalYear: int) =
        task {
            // 財務会計サービスからデータを取得
            let! data = financialDataPort.FetchFinancialDataByFiscalYearAsync(fiscalYear)

            // 財務比率を計算
            let ratios = FinancialRatios.calculate data

            let now = DateTime.UtcNow

            // キャッシュに保存
            let cache = {
                Id = None
                FiscalYear = fiscalYear
                Data = data
                Ratios = ratios
                CachedAt = now
            }
            let! _ = cacheRepository.SaveAsync(cache)

            return {
                FiscalYear = fiscalYear
                Data = data
                Ratios = ratios
                AnalyzedAt = now
            }
        }

    interface IFinancialAnalysisUseCase with
        member _.AnalyzeAsync(request: AnalyzeFinancialDataRequest) =
            task {
                try
                    // キャッシュを使用する場合は先にキャッシュを確認
                    if request.UseCache then
                        let! cached = cacheRepository.GetByFiscalYearAsync(request.FiscalYear)
                        match cached with
                        | Some cache ->
                            return Ok {
                                FiscalYear = cache.FiscalYear
                                Data = cache.Data
                                Ratios = cache.Ratios
                                AnalyzedAt = cache.CachedAt
                            }
                        | None ->
                            let! result = analyzeFromService request.FiscalYear
                            return Ok result
                    else
                        let! result = analyzeFromService request.FiscalYear
                        return Ok result
                with
                | ex ->
                    return Error $"財務分析に失敗しました: {ex.Message}"
            }

        member _.GetCachedAnalysisAsync(fiscalYear: int) =
            task {
                let! cached = cacheRepository.GetByFiscalYearAsync(fiscalYear)
                return
                    cached
                    |> Option.map (fun cache -> {
                        FiscalYear = cache.FiscalYear
                        Data = cache.Data
                        Ratios = cache.Ratios
                        AnalyzedAt = cache.CachedAt
                    })
            }

        member _.InvalidateCacheAsync(fiscalYear: int) =
            cacheRepository.DeleteByFiscalYearAsync(fiscalYear)
