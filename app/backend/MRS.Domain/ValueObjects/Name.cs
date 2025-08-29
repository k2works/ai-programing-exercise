namespace MRS.Domain.ValueObjects;

/// <summary>
/// 名前を表すバリューオブジェクト
/// </summary>
public sealed record Name
{
    private const int MaxLength = 100;

    /// <summary>
    /// 名前の値
    /// </summary>
    public string Value { get; }

    /// <summary>
    /// Nameを初期化します
    /// </summary>
    /// <param name="value">名前の値</param>
    /// <exception cref="ArgumentException">無効な値の場合</exception>
    public Name(string value)
    {
        if (string.IsNullOrWhiteSpace(value))
            throw new ArgumentException("名前は必須です。", nameof(value));

        var trimmedValue = value.Trim();
        
        if (string.IsNullOrEmpty(trimmedValue))
            throw new ArgumentException("名前は必須です。", nameof(value));

        if (trimmedValue.Length > MaxLength)
            throw new ArgumentException($"名前は{MaxLength}文字以内で入力してください。", nameof(value));

        Value = trimmedValue;
    }

    /// <summary>
    /// 文字列表現を返します
    /// </summary>
    /// <returns>名前の値</returns>
    public override string ToString() => Value;

    /// <summary>
    /// Nameから文字列への暗黙的変換
    /// </summary>
    /// <param name="name">変換するNameインスタンス</param>
    /// <returns>名前の値</returns>
    public static implicit operator string(Name name) => name.Value;
}
