using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using MRS.Application.Services;
using MRS.Application.DTOs;
using System.Security.Claims;

namespace MRS.Api.Controllers;

/// <summary>
/// 予約管理API
/// </summary>
[ApiController]
[Route("api/[controller]")]
[Authorize] // 認証済みユーザーのみアクセス可能
public class ReservationsController : ControllerBase
{
    private readonly IReservationService _reservationService;

    /// <summary>
    /// ReservationsControllerのコンストラクタ
    /// </summary>
    /// <param name="reservationService">予約サービス</param>
    public ReservationsController(IReservationService reservationService)
    {
        _reservationService = reservationService ?? throw new ArgumentNullException(nameof(reservationService));
    }

    /// <summary>
    /// 予約作成
    /// </summary>
    /// <param name="request">予約作成リクエスト</param>
    /// <returns>作成された予約の情報</returns>
    /// <response code="201">予約の作成に成功</response>
    /// <response code="400">リクエストデータが無効</response>
    /// <response code="409">予約が重複している</response>
    [HttpPost]
    [ProducesResponseType(StatusCodes.Status201Created, Type = typeof(ReservationDto))]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    [ProducesResponseType(StatusCodes.Status409Conflict)]
    public async Task<ActionResult<ReservationDto>> CreateReservation([FromBody] CreateReservationRequest request)
    {
        if (!ModelState.IsValid)
        {
            return BadRequest(ModelState);
        }

        var userId = User.FindFirst(ClaimTypes.NameIdentifier)?.Value;
        if (string.IsNullOrEmpty(userId))
        {
            return BadRequest("ユーザーIDが取得できません。");
        }

        try
        {
            var requestWithUserId = request with { UserId = userId };
            var reservation = await _reservationService.CreateReservationAsync(requestWithUserId);
            return CreatedAtAction(nameof(GetReservationById), new { reservationId = reservation.ReservationId }, reservation);
        }
        catch (ArgumentException ex)
        {
            return BadRequest(ex.Message);
        }
        catch (InvalidOperationException ex)
        {
            return Conflict(ex.Message);
        }
    }

    /// <summary>
    /// 予約詳細取得
    /// </summary>
    /// <param name="reservationId">予約ID</param>
    /// <returns>指定された予約の詳細情報</returns>
    /// <response code="200">予約詳細の取得に成功</response>
    /// <response code="404">指定された予約が見つからない</response>
    [HttpGet("{reservationId}")]
    [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(ReservationDto))]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<ActionResult<ReservationDto>> GetReservationById(string reservationId)
    {
        if (string.IsNullOrWhiteSpace(reservationId))
        {
            return BadRequest("予約IDは必須です。");
        }

        try
        {
            var reservation = await _reservationService.GetReservationByIdAsync(reservationId);
            return Ok(reservation);
        }
        catch (ArgumentException)
        {
            return NotFound($"予約ID '{reservationId}' が見つかりません。");
        }
    }

    /// <summary>
    /// 予約一覧取得
    /// </summary>
    /// <param name="roomId">会議室ID（省略時は全会議室）</param>
    /// <param name="startDate">検索開始日時</param>
    /// <param name="endDate">検索終了日時</param>
    /// <returns>検索条件に一致する予約のリスト</returns>
    /// <response code="200">予約一覧の取得に成功</response>
    [HttpGet]
    [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(IEnumerable<ReservationDto>))]
    public async Task<ActionResult<IEnumerable<ReservationDto>>> GetReservations(
        [FromQuery] string? roomId = null,
        [FromQuery] DateTime? startDate = null,
        [FromQuery] DateTime? endDate = null)
    {
        try
        {
            IEnumerable<ReservationDto> reservations;

            if (!string.IsNullOrEmpty(roomId))
            {
                reservations = await _reservationService.GetReservationsByRoomIdAsync(roomId);
            }
            else if (startDate.HasValue && endDate.HasValue)
            {
                reservations = await _reservationService.GetReservationsByDateRangeAsync(startDate.Value, endDate.Value);
            }
            else
            {
                reservations = await _reservationService.GetAllReservationsAsync();
            }

            return Ok(reservations);
        }
        catch (ArgumentException ex)
        {
            return BadRequest(ex.Message);
        }
    }

    /// <summary>
    /// ユーザーの予約一覧取得
    /// </summary>
    /// <returns>現在のユーザーの予約リスト</returns>
    /// <response code="200">ユーザー予約一覧の取得に成功</response>
    [HttpGet("my")]
    [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(IEnumerable<ReservationDto>))]
    public async Task<ActionResult<IEnumerable<ReservationDto>>> GetMyReservations()
    {
        var userId = User.FindFirst(ClaimTypes.NameIdentifier)?.Value;
        if (string.IsNullOrEmpty(userId))
        {
            return BadRequest("ユーザーIDが取得できません。");
        }

        try
        {
            var reservations = await _reservationService.GetReservationsByUserIdAsync(userId);
            return Ok(reservations);
        }
        catch (ArgumentException ex)
        {
            return BadRequest(ex.Message);
        }
    }

    /// <summary>
    /// 予約更新
    /// </summary>
    /// <param name="reservationId">予約ID</param>
    /// <param name="request">予約更新リクエスト</param>
    /// <returns>更新された予約の情報</returns>
    /// <response code="200">予約の更新に成功</response>
    /// <response code="400">リクエストデータが無効</response>
    /// <response code="404">指定された予約が見つからない</response>
    /// <response code="409">楽観的ロック競合または予約重複</response>
    [HttpPut("{reservationId}")]
    [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(ReservationDto))]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    [ProducesResponseType(StatusCodes.Status409Conflict)]
    public async Task<ActionResult<ReservationDto>> UpdateReservation(string reservationId, [FromBody] UpdateReservationRequest request, [FromHeader(Name = "If-Match")] int expectedRowVersion = 0)
    {
        if (!ModelState.IsValid)
        {
            return BadRequest(ModelState);
        }

        if (string.IsNullOrWhiteSpace(reservationId))
        {
            return BadRequest("予約IDは必須です。");
        }

        var userId = User.FindFirst(ClaimTypes.NameIdentifier)?.Value;
        if (string.IsNullOrEmpty(userId))
        {
            return BadRequest("ユーザーIDが取得できません。");
        }

        try
        {
            // ユーザーが自分の予約のみ更新できるようにチェック
            var existingReservation = await _reservationService.GetReservationByIdAsync(reservationId);
            if (existingReservation == null)
            {
                return NotFound($"予約ID '{reservationId}' が見つかりません。");
            }
            
            if (existingReservation.UserId != userId)
            {
                return Forbid("他のユーザーの予約は更新できません。");
            }

            var reservation = await _reservationService.UpdateReservationAsync(reservationId, request, expectedRowVersion);
            return Ok(reservation);
        }
        catch (ArgumentException ex)
        {
            return NotFound(ex.Message);
        }
        catch (InvalidOperationException ex)
        {
            return Conflict(ex.Message);
        }
    }

    /// <summary>
    /// 予約キャンセル
    /// </summary>
    /// <param name="reservationId">予約ID</param>
    /// <param name="request">キャンセルリクエスト</param>
    /// <returns>キャンセル結果</returns>
    /// <response code="200">予約のキャンセルに成功</response>
    /// <response code="400">リクエストデータが無効</response>
    /// <response code="401">権限がない</response>
    /// <response code="404">指定された予約が見つからない</response>
    /// <response code="409">既にキャンセル済み</response>
    [HttpPost("{reservationId}/cancel")]
    [ProducesResponseType(StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    [ProducesResponseType(StatusCodes.Status401Unauthorized)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    [ProducesResponseType(StatusCodes.Status409Conflict)]
    public async Task<IActionResult> CancelReservation(string reservationId, [FromBody] CancelReservationRequest request)
    {
        if (!ModelState.IsValid)
        {
            return BadRequest(ModelState);
        }

        if (string.IsNullOrWhiteSpace(reservationId))
        {
            return BadRequest("予約IDは必須です。");
        }

        var userId = User.FindFirst(ClaimTypes.NameIdentifier)?.Value;
        if (string.IsNullOrEmpty(userId))
        {
            return BadRequest("ユーザーIDが取得できません。");
        }

        var isAdmin = User.IsInRole("Admin");

        try
        {
            var requestWithUserId = request with { UserId = userId, IsAdmin = isAdmin };
            var success = await _reservationService.CancelReservationWithDetailsAsync(reservationId, requestWithUserId);
            
            if (success)
            {
                return Ok(new { message = "予約をキャンセルしました。" });
            }
            else
            {
                return StatusCode(StatusCodes.Status500InternalServerError, "予約のキャンセルに失敗しました。");
            }
        }
        catch (ArgumentException ex)
        {
            return BadRequest(ex.Message);
        }
        catch (UnauthorizedAccessException)
        {
            return Forbid("この予約をキャンセルする権限がありません。");
        }
        catch (InvalidOperationException ex)
        {
            if (ex.Message.Contains("not found") || ex.Message.Contains("見つからない"))
            {
                return NotFound(ex.Message);
            }
            return Conflict(ex.Message);
        }
    }
}