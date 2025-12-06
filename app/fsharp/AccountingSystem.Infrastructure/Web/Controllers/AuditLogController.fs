namespace AccountingSystem.Infrastructure.Web.Controllers

open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Mvc
open AccountingSystem.Application.Port.In
open AccountingSystem.Infrastructure.Web.Dtos

/// <summary>
/// 監査ログ REST API コントローラー（Input Adapter）
/// </summary>
[<ApiController>]
[<Route("api/v1/audit-logs")>]
[<Tags("監査ログAPI")>]
type AuditLogController(auditLogService: IAuditLogUseCase) =
    inherit ControllerBase()

    /// <summary>
    /// エンティティ別の監査ログを取得
    /// </summary>
    [<HttpGet("entity/{entityType}/{entityId}")>]
    [<ProducesResponseType(typeof<AuditLogResponse list>, StatusCodes.Status200OK)>]
    member _.GetByEntity(entityType: string, entityId: string) : Task<IActionResult> =
        task {
            let! logs = auditLogService.GetByEntityAsync(entityType, entityId)
            let response = logs |> List.map AuditLogResponse.fromDomain
            return OkObjectResult(response) :> IActionResult
        }

    /// <summary>
    /// ユーザー別の監査ログを取得
    /// </summary>
    [<HttpGet("user/{userId}")>]
    [<ProducesResponseType(typeof<AuditLogResponse list>, StatusCodes.Status200OK)>]
    member _.GetByUser(
        userId: string,
        [<FromQuery>] startDate: DateTime,
        [<FromQuery>] endDate: DateTime) : Task<IActionResult> =
        task {
            let! logs = auditLogService.GetByUserAsync(userId, startDate, endDate)
            let response = logs |> List.map AuditLogResponse.fromDomain
            return OkObjectResult(response) :> IActionResult
        }

    /// <summary>
    /// 期間内のすべての監査ログを取得
    /// </summary>
    [<HttpGet>]
    [<ProducesResponseType(typeof<AuditLogResponse list>, StatusCodes.Status200OK)>]
    member _.GetByDateRange(
        [<FromQuery>] startDate: DateTime,
        [<FromQuery>] endDate: DateTime) : Task<IActionResult> =
        task {
            let! logs = auditLogService.GetByDateRangeAsync(startDate, endDate)
            let response = logs |> List.map AuditLogResponse.fromDomain
            return OkObjectResult(response) :> IActionResult
        }

    /// <summary>
    /// アクション種別で監査ログを検索
    /// </summary>
    [<HttpGet("action/{actionType}")>]
    [<ProducesResponseType(typeof<AuditLogResponse list>, StatusCodes.Status200OK)>]
    member _.GetByAction(
        actionType: string,
        [<FromQuery>] startDate: DateTime,
        [<FromQuery>] endDate: DateTime) : Task<IActionResult> =
        task {
            let! logs = auditLogService.GetByActionAsync(actionType, startDate, endDate)
            let response = logs |> List.map AuditLogResponse.fromDomain
            return OkObjectResult(response) :> IActionResult
        }

    /// <summary>
    /// 監査ログを記録（通常はシステム内部から呼び出される）
    /// </summary>
    [<HttpPost>]
    [<ProducesResponseType(typeof<AuditLogResponse>, StatusCodes.Status201Created)>]
    [<ProducesResponseType(typeof<ErrorResponse>, StatusCodes.Status400BadRequest)>]
    member this.RecordAuditLog([<FromBody>] request: CreateAuditLogRequest) : Task<IActionResult> =
        task {
            match CreateAuditLogRequest.toDomain request with
            | Some auditLog ->
                let! recorded = auditLogService.RecordAsync(auditLog)
                return
                    this.CreatedAtAction(
                        nameof this.GetByEntity,
                        {| entityType = recorded.EntityType; entityId = recorded.EntityId |},
                        AuditLogResponse.fromDomain recorded) :> IActionResult
            | None ->
                return BadRequestObjectResult({| Error = "無効なアクション種別です" |}) :> IActionResult
        }
