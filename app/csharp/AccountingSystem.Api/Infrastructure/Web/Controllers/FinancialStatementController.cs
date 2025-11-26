using AccountingSystem.Infrastructure.Web.Dtos;
using AccountingSystem.Domain.Models;
using Microsoft.AspNetCore.Mvc;

namespace AccountingSystem.Infrastructure.Web.Controllers;

/// <summary>
/// 財務諸表 REST API コントローラー
/// </summary>
[ApiController]
[Route("api/v1/financial-statements")]
[Tags("財務諸表API")]
public class FinancialStatementController : ControllerBase
{
    private readonly IFinancialStatementService _financialStatementService;

    public FinancialStatementController(IFinancialStatementService financialStatementService)
    {
        _financialStatementService = financialStatementService;
    }

    /// <summary>
    /// 貸借対照表を取得
    /// </summary>
    /// <param name="asOfDate">基準日（YYYY-MM-DD形式）</param>
    /// <returns>貸借対照表</returns>
    [HttpGet("balance-sheet")]
    [ProducesResponseType(typeof(BalanceSheetResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status400BadRequest)]
    public async Task<IActionResult> GetBalanceSheet([FromQuery] DateOnly asOfDate)
    {
        var balanceSheet = await _financialStatementService.GenerateBalanceSheetAsync(asOfDate);
        return Ok(BalanceSheetResponse.From(balanceSheet));
    }

    /// <summary>
    /// 損益計算書を取得
    /// </summary>
    /// <param name="fromDate">開始日（YYYY-MM-DD形式）</param>
    /// <param name="toDate">終了日（YYYY-MM-DD形式）</param>
    /// <returns>損益計算書</returns>
    [HttpGet("income-statement")]
    [ProducesResponseType(typeof(IncomeStatementResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status400BadRequest)]
    public async Task<IActionResult> GetIncomeStatement(
        [FromQuery] DateOnly fromDate,
        [FromQuery] DateOnly toDate)
    {
        var incomeStatement = await _financialStatementService.GenerateIncomeStatementAsync(fromDate, toDate);
        return Ok(IncomeStatementResponse.From(incomeStatement));
    }

    /// <summary>
    /// 財務指標を取得
    /// </summary>
    /// <param name="asOfDate">基準日（YYYY-MM-DD形式）</param>
    /// <param name="fromDate">期間開始日（YYYY-MM-DD形式）</param>
    /// <param name="toDate">期間終了日（YYYY-MM-DD形式）</param>
    /// <returns>財務指標</returns>
    [HttpGet("financial-ratios")]
    [ProducesResponseType(typeof(FinancialRatiosResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status400BadRequest)]
    public async Task<IActionResult> GetFinancialRatios(
        [FromQuery] DateOnly asOfDate,
        [FromQuery] DateOnly fromDate,
        [FromQuery] DateOnly toDate)
    {
        var balanceSheet = await _financialStatementService.GenerateBalanceSheetAsync(asOfDate);
        var incomeStatement = await _financialStatementService.GenerateIncomeStatementAsync(fromDate, toDate);
        var ratios = _financialStatementService.CalculateFinancialRatios(balanceSheet, incomeStatement);
        return Ok(FinancialRatiosResponse.From(ratios));
    }
}
