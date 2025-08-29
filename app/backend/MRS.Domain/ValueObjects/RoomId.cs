namespace MRS.Domain.ValueObjects;

/// <summary>
/// 会議室IDを表すバリューオブジェクト
/// </summary>
public sealed record RoomId
{
    private const int MaxLength = 50;

    /// <summary>
    /// 会議室IDの値
    /// </summary>
    public string Value { get; }

    /// <summary>
    /// RoomIdを初期化します
    /// </summary>
    /// <param name="value">会議室ID値</param>
    /// <exception cref="ArgumentException">無効な値の場合</exception>
    public RoomId(string value)
    {
        if (string.IsNullOrWhiteSpace(value))
            throw new ArgumentException("会議室IDは必須です。", nameof(value));

        if (value.Length > MaxLength)
            throw new ArgumentException($"会議室IDは{MaxLength}文字以内で入力してください。", nameof(value));

        Value = value;
    }

    /// <summary>
    /// 文字列表現を返します
    /// </summary>
    /// <returns>会議室ID値</returns>
    public override string ToString() => Value;

    /// <summary>
    /// RoomIdから文字列への暗黙的変換
    /// </summary>
    /// <param name="roomId">変換するRoomIdインスタンス</param>
    /// <returns>会議室IDの値</returns>
    public static implicit operator string(RoomId roomId) => roomId.Value;
}
