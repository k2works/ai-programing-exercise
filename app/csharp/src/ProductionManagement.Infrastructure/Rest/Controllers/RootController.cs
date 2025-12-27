using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;

namespace ProductionManagement.Infrastructure.Rest.Controllers;

[ApiController]
[Route("/")]
[Tags("Root")]
public class RootController : ControllerBase
{
    /// <summary>
    /// API 情報の取得
    /// </summary>
    [HttpGet]
    [ProducesResponseType(typeof(ApiInfo), StatusCodes.Status200OK)]
    public IActionResult GetRoot()
    {
        return Ok(new ApiInfo(
            Message: "生産管理システム API",
            Version: "1.0.0",
            Endpoints:
            [
                "/api/items",
                "/api/bom",
                "/api/suppliers",
                "/api/orders",
                "/api/inventory",
                "/api/work-orders",
                "/api/purchase-orders"
            ],
            Docs: "/swagger"));
    }

    /// <summary>
    /// ヘルスチェック
    /// </summary>
    [HttpGet("health")]
    [ProducesResponseType(typeof(HealthStatus), StatusCodes.Status200OK)]
    public IActionResult GetHealth()
    {
        return Ok(new HealthStatus(
            Status: "ok",
            Timestamp: DateTime.UtcNow.ToString("O")));
    }
}

public record ApiInfo(
    string Message,
    string Version,
    IReadOnlyList<string> Endpoints,
    string Docs);

public record HealthStatus(
    string Status,
    string Timestamp);
