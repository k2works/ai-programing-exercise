using MRS.Application.DTOs.Rooms;

namespace MRS.Application.Ports;

/// <summary>
/// 会議室サービスのポート（入力）
/// </summary>
public interface IRoomService
{
    /// <summary>
    /// 利用可能な会議室一覧を取得
    /// </summary>
    /// <param name="request">取得条件</param>
    /// <param name="cancellationToken">キャンセルトークン</param>
    /// <returns>予約可能会議室一覧</returns>
    Task<IEnumerable<ReservableRoomDto>> GetAvailableRoomsAsync(GetRoomsRequestDto request, CancellationToken cancellationToken = default);

    /// <summary>
    /// 会議室詳細情報を取得
    /// </summary>
    /// <param name="roomId">会議室ID</param>
    /// <param name="cancellationToken">キャンセルトークン</param>
    /// <returns>会議室詳細情報</returns>
    /// <exception cref="ArgumentException">会議室が見つからない場合</exception>
    Task<RoomDto> GetRoomByIdAsync(string roomId, CancellationToken cancellationToken = default);

    /// <summary>
    /// 全会議室一覧を取得
    /// </summary>
    /// <param name="cancellationToken">キャンセルトークン</param>
    /// <returns>全会議室一覧</returns>
    Task<IEnumerable<RoomDto>> GetAllRoomsAsync(CancellationToken cancellationToken = default);
}
