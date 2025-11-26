using AccountingSystem.Application.Ports.In;
using AccountingSystem.Domain.Models.Audit;
using AccountingSystem.Infrastructure.Web.Dtos;
using Microsoft.AspNetCore.Mvc;

namespace AccountingSystem.Infrastructure.Web.Controllers;

/// <summary>
/// 監査ログ REST API コントローラー
/// </summary>
[ApiController]
[Route("api/v1/audit-logs")]
[Tags("監査ログAPI")]
public class AuditLogController : ControllerBase
{
    private readonly IAuditLogService _auditLogService;

    public AuditLogController(IAuditLogService auditLogService)
    {
        _auditLogService = auditLogService;
    }

    /// <summary>
    /// エンティティの変更履歴を取得する
    /// </summary>
    /// <param name="entityType">エンティティ種別（Account, Journal等）</param>
    /// <param name="entityId">エンティティID</param>
    /// <returns>監査ログ一覧</returns>
    [HttpGet("entity/{entityType}/{entityId}")]
    [ProducesResponseType(typeof(List<AuditLogResponse>), StatusCodes.Status200OK)]
    public async Task<IActionResult> GetEntityHistory(
        string entityType,
        string entityId)
    {
        var logs = await _auditLogService.GetEntityHistoryAsync(entityType, entityId);
        return Ok(logs.Select(AuditLogResponse.From).ToList());
    }

    /// <summary>
    /// ユーザーの操作履歴を取得する
    /// </summary>
    /// <param name="userId">ユーザーID</param>
    /// <param name="startDate">開始日時</param>
    /// <param name="endDate">終了日時</param>
    /// <returns>監査ログ一覧</returns>
    [HttpGet("user/{userId}")]
    [ProducesResponseType(typeof(List<AuditLogResponse>), StatusCodes.Status200OK)]
    public async Task<IActionResult> GetUserActivity(
        string userId,
        [FromQuery] DateTime startDate,
        [FromQuery] DateTime endDate)
    {
        var logs = await _auditLogService.GetUserActivityAsync(userId, startDate, endDate);
        return Ok(logs.Select(AuditLogResponse.From).ToList());
    }

    /// <summary>
    /// 期間別の監査ログを取得する
    /// </summary>
    /// <param name="startDate">開始日時</param>
    /// <param name="endDate">終了日時</param>
    /// <param name="limit">取得件数上限（デフォルト: 100）</param>
    /// <returns>監査ログ一覧</returns>
    [HttpGet("period")]
    [ProducesResponseType(typeof(List<AuditLogResponse>), StatusCodes.Status200OK)]
    public async Task<IActionResult> GetAuditLogsByPeriod(
        [FromQuery] DateTime startDate,
        [FromQuery] DateTime endDate,
        [FromQuery] int limit = 100)
    {
        var logs = await _auditLogService.GetAuditLogsForPeriodAsync(startDate, endDate, limit);
        return Ok(logs.Select(AuditLogResponse.From).ToList());
    }

    /// <summary>
    /// アクション別の監査ログを取得する
    /// </summary>
    /// <param name="action">アクション種別（CREATE, UPDATE, DELETE）</param>
    /// <param name="startDate">開始日時</param>
    /// <param name="endDate">終了日時</param>
    /// <param name="limit">取得件数上限（デフォルト: 100）</param>
    /// <returns>監査ログ一覧</returns>
    [HttpGet("action/{action}")]
    [ProducesResponseType(typeof(List<AuditLogResponse>), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status400BadRequest)]
    public async Task<IActionResult> GetAuditLogsByAction(
        string action,
        [FromQuery] DateTime startDate,
        [FromQuery] DateTime endDate,
        [FromQuery] int limit = 100)
    {
        if (!Enum.TryParse<AuditAction>(action, true, out var auditAction))
        {
            return BadRequest(new ErrorResponse
            {
                Error = "INVALID_ACTION",
                Message = $"無効なアクション種別です: {action}。有効な値: CREATE, UPDATE, DELETE"
            });
        }

        var logs = await _auditLogService.GetAuditLogsByActionAsync(auditAction, startDate, endDate, limit);
        return Ok(logs.Select(AuditLogResponse.From).ToList());
    }
}
