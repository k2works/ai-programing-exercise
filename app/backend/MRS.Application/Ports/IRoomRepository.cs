using MRS.Domain.Entities;
using MRS.Domain.ValueObjects;

namespace MRS.Application.Ports;

/// <summary>
/// 会議室リポジトリのポート（出力）
/// </summary>
public interface IRoomRepository
{
    /// <summary>
    /// 会議室IDで会議室を取得
    /// </summary>
    /// <param name="roomId">会議室ID</param>
    /// <param name="cancellationToken">キャンセルトークン</param>
    /// <returns>会議室エンティティ（見つからない場合はnull）</returns>
    Task<Room?> GetByIdAsync(RoomId roomId, CancellationToken cancellationToken = default);

    /// <summary>
    /// 全会議室を取得
    /// </summary>
    /// <param name="cancellationToken">キャンセルトークン</param>
    /// <returns>全会議室一覧</returns>
    Task<IEnumerable<Room>> GetAllAsync(CancellationToken cancellationToken = default);

    /// <summary>
    /// 指定日の利用可能会議室を取得
    /// </summary>
    /// <param name="date">対象日</param>
    /// <param name="cancellationToken">キャンセルトークン</param>
    /// <returns>利用可能会議室一覧</returns>
    Task<IEnumerable<ReservableRoom>> GetAvailableRoomsAsync(DateTime targetDate, CancellationToken cancellationToken = default);

    /// <summary>
    /// 会議室を追加
    /// </summary>
    /// <param name="room">追加する会議室</param>
    /// <param name="cancellationToken">キャンセルトークン</param>
    /// <returns>完了タスク</returns>
    Task AddRoomAsync(Room room, CancellationToken cancellationToken = default);

    /// <summary>
    /// 予約可能会議室を追加
    /// </summary>
    /// <param name="reservableRoom">追加する予約可能会議室</param>
    /// <param name="cancellationToken">キャンセルトークン</param>
    /// <returns>完了タスク</returns>
    Task AddReservableRoomAsync(ReservableRoom reservableRoom, CancellationToken cancellationToken = default);
}
