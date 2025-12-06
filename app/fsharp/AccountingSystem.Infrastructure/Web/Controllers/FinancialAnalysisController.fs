namespace AccountingSystem.Infrastructure.Web.Controllers

open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Mvc
open AccountingSystem.Application.Port.In
open AccountingSystem.Infrastructure.Web.Dtos

/// <summary>
/// 財務分析 REST API コントローラー（Input Adapter）
/// D社の事例に基づく包括的な財務分析機能を提供
/// </summary>
[<ApiController>]
[<Route("api/v1/financial-analysis")>]
[<Tags("財務分析API")>]
type FinancialAnalysisController(financialAnalysisService: IFinancialAnalysisUseCase) =
    inherit ControllerBase()

    /// <summary>
    /// 指定した会計年度の財務データを取得
    /// </summary>
    [<HttpGet("data/{fiscalYear:int}")>]
    [<ProducesResponseType(typeof<FinancialDataResponse>, StatusCodes.Status200OK)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status404NotFound)>]
    member this.GetFinancialData(fiscalYear: int) : Task<IActionResult> =
        task {
            let! result = financialAnalysisService.GetFinancialDataAsync(fiscalYear)
            match result with
            | Ok data ->
                return this.Ok(FinancialDataResponse.from data) :> IActionResult
            | Error msg ->
                return this.NotFound(ErrorResponse.create 404 "NotFound" msg) :> IActionResult
        }

    /// <summary>
    /// 指定した会計年度の包括的な財務指標を取得
    /// </summary>
    [<HttpGet("ratios/{fiscalYear:int}")>]
    [<ProducesResponseType(typeof<ComprehensiveFinancialRatiosResponse>, StatusCodes.Status200OK)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status404NotFound)>]
    member this.GetComprehensiveRatios(fiscalYear: int) : Task<IActionResult> =
        task {
            let! result = financialAnalysisService.CalculateComprehensiveRatiosAsync(fiscalYear)
            match result with
            | Ok ratios ->
                return this.Ok(ComprehensiveFinancialRatiosResponse.from ratios) :> IActionResult
            | Error msg ->
                return this.NotFound(ErrorResponse.create 404 "NotFound" msg) :> IActionResult
        }

    /// <summary>
    /// 複数年度の財務指標を比較
    /// </summary>
    [<HttpPost("compare")>]
    [<ProducesResponseType(typeof<ComprehensiveFinancialRatiosResponse array>, StatusCodes.Status200OK)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status400BadRequest)>]
    member this.CompareRatios([<FromBody>] request: CompareRatiosRequest) : Task<IActionResult> =
        task {
            if request.FiscalYears = null || request.FiscalYears.Length = 0 then
                return this.BadRequest(ErrorResponse.create 400 "BadRequest" "会計年度を指定してください") :> IActionResult
            else
                let fiscalYears = request.FiscalYears |> Array.toList
                let! result = financialAnalysisService.CompareFinancialRatiosAsync(fiscalYears)
                match result with
                | Ok ratiosList ->
                    let responses = ratiosList |> List.map ComprehensiveFinancialRatiosResponse.from |> List.toArray
                    return this.Ok(responses) :> IActionResult
                | Error msg ->
                    return this.BadRequest(ErrorResponse.create 400 "BadRequest" msg) :> IActionResult
        }

    /// <summary>
    /// 収益性分析を実行
    /// </summary>
    [<HttpGet("profitability/{fiscalYear:int}")>]
    [<ProducesResponseType(typeof<ProfitabilityAnalysisResponse>, StatusCodes.Status200OK)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status404NotFound)>]
    member this.AnalyzeProfitability(fiscalYear: int) : Task<IActionResult> =
        task {
            let! result = financialAnalysisService.AnalyzeProfitabilityAsync(fiscalYear)
            match result with
            | Ok analysis ->
                return this.Ok(ProfitabilityAnalysisResponse.from analysis) :> IActionResult
            | Error msg ->
                return this.NotFound(ErrorResponse.create 404 "NotFound" msg) :> IActionResult
        }

    /// <summary>
    /// 効率性分析を実行
    /// </summary>
    [<HttpGet("efficiency/{fiscalYear:int}")>]
    [<ProducesResponseType(typeof<EfficiencyAnalysisResponse>, StatusCodes.Status200OK)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status404NotFound)>]
    member this.AnalyzeEfficiency(fiscalYear: int) : Task<IActionResult> =
        task {
            let! result = financialAnalysisService.AnalyzeEfficiencyAsync(fiscalYear)
            match result with
            | Ok analysis ->
                return this.Ok(EfficiencyAnalysisResponse.from analysis) :> IActionResult
            | Error msg ->
                return this.NotFound(ErrorResponse.create 404 "NotFound" msg) :> IActionResult
        }

    /// <summary>
    /// 安全性分析を実行
    /// </summary>
    [<HttpGet("safety/{fiscalYear:int}")>]
    [<ProducesResponseType(typeof<SafetyAnalysisResponse>, StatusCodes.Status200OK)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status404NotFound)>]
    member this.AnalyzeSafety(fiscalYear: int) : Task<IActionResult> =
        task {
            let! result = financialAnalysisService.AnalyzeSafetyAsync(fiscalYear)
            match result with
            | Ok analysis ->
                return this.Ok(SafetyAnalysisResponse.from analysis) :> IActionResult
            | Error msg ->
                return this.NotFound(ErrorResponse.create 404 "NotFound" msg) :> IActionResult
        }
