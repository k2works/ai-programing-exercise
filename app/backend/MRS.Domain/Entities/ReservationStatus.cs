namespace MRS.Domain.Entities;

/// <summary>
/// 予約の状態を表す列挙型
/// </summary>
public enum ReservationStatus
{
    /// <summary>
    /// 確定済み
    /// </summary>
    Confirmed = 1,

    /// <summary>
    /// ユーザーによりキャンセル済み
    /// </summary>
    Cancelled = 2,

    /// <summary>
    /// 管理者によりキャンセル済み
    /// </summary>
    CancelledByAdmin = 3
}