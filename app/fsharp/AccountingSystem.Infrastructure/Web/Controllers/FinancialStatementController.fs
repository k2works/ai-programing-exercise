namespace AccountingSystem.Infrastructure.Web.Controllers

open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Mvc
open AccountingSystem.Application.Port.In
open AccountingSystem.Infrastructure.Web.Dtos

/// <summary>
/// 財務諸表 REST API コントローラー（Input Adapter）
/// </summary>
[<ApiController>]
[<Route("api/v1/financial-statements")>]
[<Tags("財務諸表API")>]
type FinancialStatementController(financialStatementService: IFinancialStatementUseCase) =
    inherit ControllerBase()

    /// <summary>
    /// 貸借対照表を取得
    /// </summary>
    [<HttpGet("balance-sheet")>]
    [<ProducesResponseType(typeof<BalanceSheetResponse>, StatusCodes.Status200OK)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status400BadRequest)>]
    member _.GetBalanceSheet([<FromQuery>] asOfDate: DateTime) : Task<IActionResult> =
        task {
            let! balanceSheet = financialStatementService.GenerateBalanceSheetAsync(asOfDate)
            return OkObjectResult(BalanceSheetResponse.from balanceSheet) :> IActionResult
        }

    /// <summary>
    /// 損益計算書を取得
    /// </summary>
    [<HttpGet("income-statement")>]
    [<ProducesResponseType(typeof<IncomeStatementResponse>, StatusCodes.Status200OK)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status400BadRequest)>]
    member _.GetIncomeStatement(
        [<FromQuery>] fromDate: DateTime,
        [<FromQuery>] toDate: DateTime) : Task<IActionResult> =
        task {
            let! incomeStatement = financialStatementService.GenerateIncomeStatementAsync(fromDate, toDate)
            return OkObjectResult(IncomeStatementResponse.from incomeStatement) :> IActionResult
        }

    /// <summary>
    /// 財務指標を取得
    /// </summary>
    [<HttpGet("ratios")>]
    [<ProducesResponseType(typeof<FinancialRatiosResponse>, StatusCodes.Status200OK)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status400BadRequest)>]
    member _.GetFinancialRatios(
        [<FromQuery>] asOfDate: DateTime,
        [<FromQuery>] fromDate: DateTime,
        [<FromQuery>] toDate: DateTime) : Task<IActionResult> =
        task {
            let! ratios = financialStatementService.CalculateFinancialRatiosAsync(asOfDate, fromDate, toDate)
            return OkObjectResult(FinancialRatiosResponse.from ratios) :> IActionResult
        }
