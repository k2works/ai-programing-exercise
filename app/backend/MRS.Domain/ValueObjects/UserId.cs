namespace MRS.Domain.ValueObjects;

/// <summary>
/// ユーザーIDを表すバリューオブジェクト
/// </summary>
public sealed record UserId
{
    private const int MaxLength = 50;

    /// <summary>
    /// ユーザーIDの値
    /// </summary>
    public string Value { get; }

    /// <summary>
    /// UserIdを初期化します
    /// </summary>
    /// <param name="value">ユーザーID値</param>
    /// <exception cref="ArgumentException">無効な値の場合</exception>
    public UserId(string value)
    {
        if (string.IsNullOrWhiteSpace(value))
            throw new ArgumentException("ユーザーIDは必須です。", nameof(value));

        if (value.Length > MaxLength)
            throw new ArgumentException($"ユーザーIDは{MaxLength}文字以内で入力してください。", nameof(value));

        Value = value;
    }

    /// <summary>
    /// 文字列表現を返します
    /// </summary>
    /// <returns>ユーザーID値</returns>
    public override string ToString() => Value;
}
