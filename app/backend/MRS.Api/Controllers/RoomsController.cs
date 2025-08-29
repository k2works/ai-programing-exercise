using Microsoft.AspNetCore.Mvc;
using MRS.Application.Ports;
using MRS.Application.DTOs.Rooms;

namespace MRS.Api.Controllers;

/// <summary>
/// 会議室管理API
/// </summary>
[ApiController]
[Route("api/[controller]")]
public class RoomsController : ControllerBase
{
    private readonly IRoomService _roomService;

    /// <summary>
    /// RoomsControllerのコンストラクタ
    /// </summary>
    /// <param name="roomService">会議室サービス</param>
    public RoomsController(IRoomService roomService)
    {
        _roomService = roomService ?? throw new ArgumentNullException(nameof(roomService));
    }

    /// <summary>
    /// 全会議室一覧取得
    /// </summary>
    /// <returns>全会議室のリスト</returns>
    /// <response code="200">会議室一覧の取得に成功</response>
    [HttpGet]
    [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(IEnumerable<RoomDto>))]
    public async Task<ActionResult<IEnumerable<RoomDto>>> GetAllRooms()
    {
        var rooms = await _roomService.GetAllRoomsAsync();
        return Ok(rooms);
    }

    /// <summary>
    /// 会議室詳細取得
    /// </summary>
    /// <param name="roomId">会議室ID</param>
    /// <returns>指定された会議室の詳細情報</returns>
    /// <response code="200">会議室詳細の取得に成功</response>
    /// <response code="404">指定された会議室が見つからない</response>
    [HttpGet("{roomId}")]
    [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(RoomDto))]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<ActionResult<RoomDto>> GetRoomById(string roomId)
    {
        if (string.IsNullOrWhiteSpace(roomId))
        {
            return BadRequest("会議室IDは必須です。");
        }

        try
        {
            var room = await _roomService.GetRoomByIdAsync(roomId);
            return Ok(room);
        }
        catch (ArgumentException)
        {
            return NotFound($"会議室ID '{roomId}' が見つかりません。");
        }
    }

    /// <summary>
    /// 利用可能会議室検索
    /// </summary>
    /// <param name="date">検索対象日時（省略時は今日）</param>
    /// <param name="minCapacity">最小収容人数</param>
    /// <param name="availableOnly">利用可能な会議室のみ検索するか</param>
    /// <returns>検索条件に一致する利用可能会議室のリスト</returns>
    /// <response code="200">利用可能会議室の検索に成功</response>
    [HttpGet("available")]
    [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(IEnumerable<ReservableRoomDto>))]
    public async Task<ActionResult<IEnumerable<ReservableRoomDto>>> GetAvailableRooms(
        [FromQuery] DateTime? date = null,
        [FromQuery] int? minCapacity = null, 
        [FromQuery] bool availableOnly = true)
    {
        // 最小収容人数の検証
        if (minCapacity.HasValue && minCapacity.Value <= 0)
        {
            return BadRequest("最小収容人数は1以上である必要があります。");
        }

        var request = new GetRoomsRequestDto
        {
            Date = date,
            MinCapacity = minCapacity,
            AvailableOnly = availableOnly
        };

        var rooms = await _roomService.GetAvailableRoomsAsync(request);
        return Ok(rooms);
    }
}
