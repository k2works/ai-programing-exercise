using ManagementAccounting.Application.Ports.In;
using ManagementAccounting.Domain.Entities;
using Microsoft.AspNetCore.Mvc;

namespace ManagementAccounting.Api.Controllers;

/// <summary>
/// 財務分析コントローラー
/// </summary>
[ApiController]
[Route("api/financial-analysis")]
public class FinancialAnalysisController : ControllerBase
{
    private readonly IAnalyzeFinancialDataUseCase _analyzeUseCase;

    public FinancialAnalysisController(IAnalyzeFinancialDataUseCase analyzeUseCase)
    {
        _analyzeUseCase = analyzeUseCase;
    }

    /// <summary>
    /// 指定された会計年度の財務分析を実行
    /// </summary>
    [HttpGet("{fiscalYear}")]
    public async Task<ActionResult<FinancialAnalysisResult>> Analyze(int fiscalYear)
    {
        var result = await _analyzeUseCase.AnalyzeAsync(fiscalYear);
        return Ok(result);
    }

    /// <summary>
    /// 複数期間の比較分析を実行
    /// </summary>
    [HttpGet("compare")]
    public async Task<ActionResult<List<FinancialAnalysisResult>>> Compare(
        [FromQuery] List<int> fiscalYears)
    {
        if (fiscalYears == null || fiscalYears.Count == 0)
        {
            return BadRequest("少なくとも1つの会計年度を指定してください");
        }

        var results = await _analyzeUseCase.CompareAsync(fiscalYears);
        return Ok(results);
    }
}
