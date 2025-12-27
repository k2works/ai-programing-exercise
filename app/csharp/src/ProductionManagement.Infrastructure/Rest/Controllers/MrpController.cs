using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using ProductionManagement.Application.Services;
using ProductionManagement.Infrastructure.Rest.Dto;

namespace ProductionManagement.Infrastructure.Rest.Controllers;

/// <summary>
/// MRP 実行 Controller（Input Adapter）
/// </summary>
[ApiController]
[Route("api/mrp")]
[Tags("MRP")]
public class MrpController : ControllerBase
{
    private readonly MrpService _mrpService;

    public MrpController(MrpService mrpService)
    {
        _mrpService = mrpService;
    }

    /// <summary>
    /// 所要量展開の実行
    /// </summary>
    [HttpPost("explode")]
    [ProducesResponseType(typeof(IReadOnlyList<RequirementResponse>), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    public async Task<IActionResult> ExplodeRequirements([FromBody] MrpExecuteRequest request)
    {
        var requirements = await _mrpService.ExplodeRequirementsAsync(request.OrderId);
        return Ok(requirements.Select(RequirementResponse.From).ToList());
    }

    /// <summary>
    /// 在庫からの引当
    /// </summary>
    [HttpPost("allocate")]
    [ProducesResponseType(typeof(AllocationResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    public async Task<IActionResult> AllocateFromInventory([FromBody] AllocateInventoryRequest request)
    {
        var allocation = await _mrpService.AllocateFromInventoryAsync(
            request.RequirementId,
            request.InventoryQuantity
        );
        return Ok(AllocationResponse.From(allocation));
    }
}
