namespace MRS.Application.DTOs.Rooms;

/// <summary>
/// 会議室DTO
/// </summary>
public record RoomDto
{
    /// <summary>
    /// 会議室ID
    /// </summary>
    public required string RoomId { get; init; }

    /// <summary>
    /// 会議室名
    /// </summary>
    public required string RoomName { get; init; }

    /// <summary>
    /// 収容人数
    /// </summary>
    public required int Capacity { get; init; }

    /// <summary>
    /// アクティブ状態
    /// </summary>
    public required bool IsActive { get; init; }
}

/// <summary>
/// 予約可能会議室DTO
/// </summary>
public record ReservableRoomDto
{
    /// <summary>
    /// 予約可能会議室ID
    /// </summary>
    public required string ReservableRoomId { get; init; }

    /// <summary>
    /// 会議室ID
    /// </summary>
    public required string RoomId { get; init; }

    /// <summary>
    /// 会議室名
    /// </summary>
    public required string RoomName { get; init; }

    /// <summary>
    /// 収容人数
    /// </summary>
    public required int Capacity { get; init; }

    /// <summary>
    /// 利用可能状態
    /// </summary>
    public required bool IsAvailable { get; init; }

    /// <summary>
    /// 対象日
    /// </summary>
    public required DateTime Date { get; init; }
}

/// <summary>
/// 会議室一覧取得リクエストDTO
/// </summary>
public record GetRoomsRequestDto
{
    /// <summary>
    /// 対象日（指定しない場合は本日）
    /// </summary>
    public DateTime? Date { get; init; }

    /// <summary>
    /// 最小収容人数
    /// </summary>
    public int? MinCapacity { get; init; }

    /// <summary>
    /// 利用可能のみ取得するか
    /// </summary>
    public bool AvailableOnly { get; init; } = true;
}
