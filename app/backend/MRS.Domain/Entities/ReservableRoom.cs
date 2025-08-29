using MRS.Domain.ValueObjects;

namespace MRS.Domain.Entities;

/// <summary>
/// 予約可能会議室エンティティ
/// </summary>
public class ReservableRoom : IEquatable<ReservableRoom>
{
    /// <summary>
    /// 予約可能会議室ID
    /// </summary>
    public ReservableRoomId ReservableRoomId { get; }

    /// <summary>
    /// 会議室ID
    /// </summary>
    public RoomId RoomId { get; }

    /// <summary>
    /// 会議室名
    /// </summary>
    public Name RoomName { get; private set; }

    /// <summary>
    /// 利用可能状態
    /// </summary>
    public bool IsAvailable { get; private set; }

    /// <summary>
    /// 作成日時
    /// </summary>
    public DateTime CreatedAt { get; }

    /// <summary>
    /// 更新日時
    /// </summary>
    public DateTime UpdatedAt { get; private set; }

    /// <summary>
    /// ReservableRoomエンティティを初期化します
    /// </summary>
    /// <param name="reservableRoomId">予約可能会議室ID</param>
    /// <param name="roomId">会議室ID</param>
    /// <param name="roomName">会議室名</param>
    /// <exception cref="ArgumentNullException">必須パラメータがnullの場合</exception>
    public ReservableRoom(ReservableRoomId reservableRoomId, RoomId roomId, Name roomName)
    {
        ReservableRoomId = reservableRoomId ?? throw new ArgumentNullException(nameof(reservableRoomId));
        RoomId = roomId ?? throw new ArgumentNullException(nameof(roomId));
        RoomName = roomName ?? throw new ArgumentNullException(nameof(roomName));

        IsAvailable = true;
        CreatedAt = DateTime.UtcNow;
        UpdatedAt = DateTime.UtcNow;
    }

    /// <summary>
    /// 会議室名を変更します
    /// </summary>
    /// <param name="newRoomName">新しい会議室名</param>
    /// <exception cref="ArgumentNullException">会議室名がnullの場合</exception>
    public void ChangeRoomName(Name newRoomName)
    {
        RoomName = newRoomName ?? throw new ArgumentNullException(nameof(newRoomName));
        UpdatedAt = DateTime.UtcNow;
    }

    /// <summary>
    /// 予約可能会議室を利用不可にします
    /// </summary>
    public void MakeUnavailable()
    {
        IsAvailable = false;
        UpdatedAt = DateTime.UtcNow;
    }

    /// <summary>
    /// 予約可能会議室を利用可能にします
    /// </summary>
    public void MakeAvailable()
    {
        IsAvailable = true;
        UpdatedAt = DateTime.UtcNow;
    }

    /// <summary>
    /// 予約可能かを判定します
    /// </summary>
    /// <returns>利用可能な場合true</returns>
    public bool IsAvailableForReservation()
    {
        return IsAvailable;
    }

    /// <summary>
    /// 等価性の比較（ReservableRoomIdベース）
    /// </summary>
    /// <param name="other">比較対象のReservableRoom</param>
    /// <returns>ReservableRoomIdが同じ場合true</returns>
    public bool Equals(ReservableRoom? other)
    {
        if (other is null) return false;
        if (ReferenceEquals(this, other)) return true;
        return ReservableRoomId.Equals(other.ReservableRoomId);
    }

    /// <summary>
    /// 等価性の比較
    /// </summary>
    /// <param name="obj">比較対象のオブジェクト</param>
    /// <returns>同じReservableRoomの場合true</returns>
    public override bool Equals(object? obj)
    {
        return Equals(obj as ReservableRoom);
    }

    /// <summary>
    /// ハッシュコードを取得します
    /// </summary>
    /// <returns>ReservableRoomIdのハッシュコード</returns>
    public override int GetHashCode()
    {
        return ReservableRoomId.GetHashCode();
    }

    /// <summary>
    /// 等価演算子
    /// </summary>
    public static bool operator ==(ReservableRoom? left, ReservableRoom? right)
    {
        return EqualityComparer<ReservableRoom>.Default.Equals(left, right);
    }

    /// <summary>
    /// 非等価演算子
    /// </summary>
    public static bool operator !=(ReservableRoom? left, ReservableRoom? right)
    {
        return !(left == right);
    }

    /// <summary>
    /// 文字列表現を返します
    /// </summary>
    /// <returns>予約可能会議室情報の文字列表現</returns>
    public override string ToString()
    {
        return $"ReservableRoom {{ ReservableRoomId: {ReservableRoomId}, RoomId: {RoomId}, RoomName: {RoomName}, IsAvailable: {IsAvailable} }}";
    }
}
