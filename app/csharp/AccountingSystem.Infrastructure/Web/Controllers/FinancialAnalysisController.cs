using Microsoft.AspNetCore.Http;
using AccountingSystem.Application.Ports.In;
using AccountingSystem.Infrastructure.Web.Dtos;
using Microsoft.AspNetCore.Mvc;

namespace AccountingSystem.Infrastructure.Web.Controllers;

/// <summary>
/// 財務分析 REST API コントローラー
/// D社の財務データに基づく各種財務指標の分析を提供
/// </summary>
[ApiController]
[Route("api/v1/financial-analysis")]
[Tags("財務分析API")]
public class FinancialAnalysisController : ControllerBase
{
    private readonly IFinancialAnalysisService _financialAnalysisService;

    public FinancialAnalysisController(IFinancialAnalysisService financialAnalysisService)
    {
        _financialAnalysisService = financialAnalysisService;
    }

    /// <summary>
    /// 指定した決算期の財務分析を取得
    /// </summary>
    /// <param name="fiscalYear">決算期（例: 2021, 2022）</param>
    /// <returns>財務分析結果（収益性・効率性・安全性指標）</returns>
    [HttpGet("{fiscalYear:int}")]
    [ProducesResponseType(typeof(FinancialAnalysisResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status400BadRequest)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status404NotFound)]
    public async Task<IActionResult> GetAnalysis(int fiscalYear)
    {
        try
        {
            var result = await _financialAnalysisService.AnalyzeAsync(fiscalYear);
            return Ok(FinancialAnalysisResponse.From(result));
        }
        catch (InvalidOperationException ex)
        {
            return NotFound(new ErrorResponse
            {
                Code = "NOT_FOUND",
                Message = ex.Message
            });
        }
        catch (ArgumentException ex)
        {
            return BadRequest(new ErrorResponse
            {
                Code = "INVALID_DATA",
                Message = ex.Message
            });
        }
    }

    /// <summary>
    /// 複数期間の財務分析を取得し、比較可能なデータを返す
    /// </summary>
    /// <param name="fromFiscalYear">開始決算期</param>
    /// <param name="toFiscalYear">終了決算期</param>
    /// <returns>期間ごとの財務分析結果のリスト</returns>
    [HttpGet]
    [ProducesResponseType(typeof(IReadOnlyList<FinancialAnalysisResponse>), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status400BadRequest)]
    public async Task<IActionResult> GetAnalysisRange(
        [FromQuery] int fromFiscalYear,
        [FromQuery] int toFiscalYear)
    {
        try
        {
            var results = await _financialAnalysisService.AnalyzeRangeAsync(fromFiscalYear, toFiscalYear);
            var responses = results.Select(FinancialAnalysisResponse.From).ToList();
            return Ok(responses);
        }
        catch (ArgumentException ex)
        {
            return BadRequest(new ErrorResponse
            {
                Code = "INVALID_PARAMETER",
                Message = ex.Message
            });
        }
    }
}
