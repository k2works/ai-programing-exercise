using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using ProductionManagement.Application.Services;

namespace ProductionManagement.Infrastructure.Rest.Controllers;

/// <summary>
/// BOM Controller（Input Adapter）
/// </summary>
[ApiController]
[Route("api/bom")]
[Tags("Bom")]
public class BomController : ControllerBase
{
    private readonly BomService _bomService;

    public BomController(BomService bomService)
    {
        _bomService = bomService;
    }

    /// <summary>
    /// 部品展開
    /// </summary>
    /// <remarks>指定した品目の BOM を再帰的に展開します</remarks>
    [HttpGet("{itemCode}/explode")]
    [ProducesResponseType(typeof(BomNode), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<IActionResult> ExplodeBom(string itemCode)
    {
        var result = await _bomService.ExplodeBomAsync(itemCode);
        return Ok(result);
    }

    /// <summary>
    /// 使用先照会
    /// </summary>
    /// <remarks>指定した品目が使用されている親品目を検索します</remarks>
    [HttpGet("{itemCode}/where-used")]
    [ProducesResponseType(typeof(IReadOnlyList<WhereUsedResult>), StatusCodes.Status200OK)]
    public async Task<IActionResult> WhereUsed(string itemCode)
    {
        var result = await _bomService.WhereUsedAsync(itemCode);
        return Ok(result);
    }
}
