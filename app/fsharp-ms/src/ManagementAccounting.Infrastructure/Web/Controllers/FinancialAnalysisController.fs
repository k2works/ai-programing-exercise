namespace ManagementAccounting.Infrastructure.Web.Controllers

open Microsoft.AspNetCore.Mvc
open ManagementAccounting.Application.Ports.In
open ManagementAccounting.Infrastructure.Web.Dtos

/// <summary>
/// 財務分析 API コントローラー
/// </summary>
[<ApiController>]
[<Route("api/financial-analysis")>]
type FinancialAnalysisController(useCase: IFinancialAnalysisUseCase) =
    inherit ControllerBase()

    /// <summary>
    /// 財務分析を実行
    /// </summary>
    [<HttpPost>]
    member this.Analyze([<FromBody>] request: AnalyzeFinancialDataRequestDto) =
        task {
            let! result = useCase.AnalyzeAsync(FinancialAnalysisDto.toRequest request)

            match result with
            | Ok analysis ->
                let response = FinancialAnalysisDto.toResultResponse analysis
                return this.Ok(response) :> IActionResult
            | Error message ->
                return this.BadRequest({| Error = message |}) :> IActionResult
        }

    /// <summary>
    /// 会計年度を指定して財務分析を実行
    /// </summary>
    [<HttpGet("{fiscalYear}")>]
    member this.AnalyzeByFiscalYear(fiscalYear: int, [<FromQuery>] useCache: bool) =
        task {
            let request: AnalyzeFinancialDataRequest = { FiscalYear = fiscalYear; UseCache = useCache }
            let! result = useCase.AnalyzeAsync(request)

            match result with
            | Ok analysis ->
                let response = FinancialAnalysisDto.toResultResponse analysis
                return this.Ok(response) :> IActionResult
            | Error message ->
                return this.BadRequest({| Error = message |}) :> IActionResult
        }

    /// <summary>
    /// キャッシュされた分析結果を取得
    /// </summary>
    [<HttpGet("{fiscalYear}/cached")>]
    member this.GetCachedAnalysis(fiscalYear: int) =
        task {
            let! cached = useCase.GetCachedAnalysisAsync(fiscalYear)

            match cached with
            | Some analysis ->
                let response = FinancialAnalysisDto.toResultResponse analysis
                return this.Ok(response) :> IActionResult
            | None ->
                return this.NotFound({| Message = $"会計年度 {fiscalYear} のキャッシュが見つかりません" |}) :> IActionResult
        }

    /// <summary>
    /// キャッシュを無効化
    /// </summary>
    [<HttpDelete("{fiscalYear}/cache")>]
    member this.InvalidateCache(fiscalYear: int) =
        task {
            let! deleted = useCase.InvalidateCacheAsync(fiscalYear)

            if deleted then
                return this.NoContent() :> IActionResult
            else
                return this.NotFound({| Message = $"会計年度 {fiscalYear} のキャッシュが見つかりません" |}) :> IActionResult
        }
