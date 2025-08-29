using MRS.Domain.ValueObjects;

namespace MRS.Domain.Entities;

/// <summary>
/// 会議室エンティティ
/// </summary>
public class Room : IEquatable<Room>
{
    private const int MinCapacity = 1;
    private const int MaxCapacity = 100;
    private const int DefaultCapacity = 10;

    /// <summary>
    /// 会議室ID
    /// </summary>
    public RoomId RoomId { get; }

    /// <summary>
    /// 会議室名
    /// </summary>
    public Name RoomName { get; private set; }

    /// <summary>
    /// 収容人数
    /// </summary>
    public int Capacity { get; private set; }

    /// <summary>
    /// アクティブ状態
    /// </summary>
    public bool IsActive { get; private set; }

    /// <summary>
    /// 作成日時
    /// </summary>
    public DateTime CreatedAt { get; }

    /// <summary>
    /// 更新日時
    /// </summary>
    public DateTime UpdatedAt { get; private set; }

    /// <summary>
    /// Roomエンティティを初期化します
    /// </summary>
    /// <param name="roomId">会議室ID</param>
    /// <param name="roomName">会議室名</param>
    /// <param name="capacity">収容人数（デフォルト：10名）</param>
    /// <exception cref="ArgumentNullException">必須パラメータがnullの場合</exception>
    /// <exception cref="ArgumentException">収容人数が無効な場合</exception>
    public Room(RoomId roomId, Name roomName, int capacity = DefaultCapacity)
    {
        RoomId = roomId ?? throw new ArgumentNullException(nameof(roomId));
        RoomName = roomName ?? throw new ArgumentNullException(nameof(roomName));

        ValidateCapacity(capacity);
        Capacity = capacity;

        IsActive = true;
        CreatedAt = DateTime.UtcNow;
        UpdatedAt = DateTime.UtcNow;
    }

    /// <summary>
    /// データベースからの復元用コンストラクタ
    /// </summary>
    /// <param name="roomId">会議室ID</param>
    /// <param name="roomName">会議室名</param>
    /// <param name="capacity">収容人数</param>
    /// <param name="isActive">アクティブ状態</param>
    /// <param name="createdAt">作成日時</param>
    /// <param name="updatedAt">更新日時</param>
    /// <exception cref="ArgumentNullException">必須パラメータがnullの場合</exception>
    /// <exception cref="ArgumentException">収容人数が無効な場合</exception>
    public Room(RoomId roomId, Name roomName, int capacity, bool isActive, DateTime createdAt, DateTime updatedAt)
    {
        RoomId = roomId ?? throw new ArgumentNullException(nameof(roomId));
        RoomName = roomName ?? throw new ArgumentNullException(nameof(roomName));

        ValidateCapacity(capacity);
        Capacity = capacity;
        IsActive = isActive;
        CreatedAt = createdAt;
        UpdatedAt = updatedAt;
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
    /// 収容人数を変更します
    /// </summary>
    /// <param name="newCapacity">新しい収容人数</param>
    /// <exception cref="ArgumentException">収容人数が無効な場合</exception>
    public void ChangeCapacity(int newCapacity)
    {
        ValidateCapacity(newCapacity);
        Capacity = newCapacity;
        UpdatedAt = DateTime.UtcNow;
    }

    /// <summary>
    /// 会議室を非アクティブ化します
    /// </summary>
    public void Deactivate()
    {
        IsActive = false;
        UpdatedAt = DateTime.UtcNow;
    }

    /// <summary>
    /// 会議室をアクティブ化します
    /// </summary>
    public void Activate()
    {
        IsActive = true;
        UpdatedAt = DateTime.UtcNow;
    }

    /// <summary>
    /// 指定人数を収容できるかを判定します
    /// </summary>
    /// <param name="attendeeCount">参加者数</param>
    /// <returns>収容可能かつアクティブな場合true</returns>
    public bool CanAccommodate(int attendeeCount)
    {
        if (!IsActive) return false;
        if (attendeeCount <= 0) return false;
        return attendeeCount <= Capacity;
    }

    /// <summary>
    /// 等価性の比較（RoomIdベース）
    /// </summary>
    /// <param name="other">比較対象のRoom</param>
    /// <returns>RoomIdが同じ場合true</returns>
    public bool Equals(Room? other)
    {
        if (other is null) return false;
        if (ReferenceEquals(this, other)) return true;
        return RoomId.Equals(other.RoomId);
    }

    /// <summary>
    /// 等価性の比較
    /// </summary>
    /// <param name="obj">比較対象のオブジェクト</param>
    /// <returns>同じRoomの場合true</returns>
    public override bool Equals(object? obj)
    {
        return Equals(obj as Room);
    }

    /// <summary>
    /// ハッシュコードを取得します
    /// </summary>
    /// <returns>RoomIdのハッシュコード</returns>
    public override int GetHashCode()
    {
        return RoomId.GetHashCode();
    }

    /// <summary>
    /// 等価演算子
    /// </summary>
    public static bool operator ==(Room? left, Room? right)
    {
        return EqualityComparer<Room>.Default.Equals(left, right);
    }

    /// <summary>
    /// 非等価演算子
    /// </summary>
    public static bool operator !=(Room? left, Room? right)
    {
        return !(left == right);
    }

    /// <summary>
    /// 文字列表現を返します
    /// </summary>
    /// <returns>会議室情報の文字列表現</returns>
    public override string ToString()
    {
        return $"Room {{ RoomId: {RoomId}, RoomName: {RoomName}, Capacity: {Capacity}, IsActive: {IsActive} }}";
    }

    /// <summary>
    /// 収容人数のバリデーション
    /// </summary>
    /// <param name="capacity">検証する収容人数</param>
    /// <exception cref="ArgumentException">無効な収容人数の場合</exception>
    private static void ValidateCapacity(int capacity)
    {
        if (capacity < MinCapacity || capacity > MaxCapacity)
            throw new ArgumentException($"収容人数は{MinCapacity}名以上{MaxCapacity}名以下で入力してください。", nameof(capacity));
    }
}
