namespace MRS.Domain.ValueObjects;

/// <summary>
/// 予約可能会議室IDバリューオブジェクト
/// </summary>
public record ReservableRoomId
{
    private const int MaxLength = 100;

    /// <summary>
    /// 予約可能会議室IDの値
    /// </summary>
    public string Value { get; }

    /// <summary>
    /// ReservableRoomIdを初期化します
    /// </summary>
    /// <param name="value">予約可能会議室IDの値</param>
    /// <exception cref="ArgumentException">値が無効な場合</exception>
    public ReservableRoomId(string value)
    {
        if (string.IsNullOrWhiteSpace(value))
            throw new ArgumentException("ReservableRoomIdは必須です。", nameof(value));

        if (value.Length > MaxLength)
            throw new ArgumentException($"ReservableRoomIdは{MaxLength}文字以下で入力してください。", nameof(value));

        Value = value;
    }

    /// <summary>
    /// 文字列表現を返します
    /// </summary>
    /// <returns>予約可能会議室IDの値</returns>
    public override string ToString() => Value;

    /// <summary>
    /// ReservableRoomIdから文字列への暗黙的変換
    /// </summary>
    /// <param name="reservableRoomId">変換元のReservableRoomId</param>
    /// <returns>予約可能会議室IDの値</returns>
    public static implicit operator string(ReservableRoomId reservableRoomId) => reservableRoomId.Value;
}
