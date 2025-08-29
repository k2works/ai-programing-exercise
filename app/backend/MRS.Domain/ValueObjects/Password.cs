using BCrypt.Net;

namespace MRS.Domain.ValueObjects;

/// <summary>
/// パスワードを表すバリューオブジェクト
/// </summary>
public sealed record Password
{
    private const int MinLength = 8;
    private const int MaxLength = 128;

    /// <summary>
    /// ハッシュ化されたパスワード値
    /// </summary>
    public string HashedValue { get; }

    /// <summary>
    /// 平文パスワードからPasswordを初期化します
    /// </summary>
    /// <param name="plainPassword">平文パスワード</param>
    /// <exception cref="ArgumentException">無効な値の場合</exception>
    public Password(string plainPassword)
    {
        ValidatePlainPassword(plainPassword);
        HashedValue = BCrypt.Net.BCrypt.HashPassword(plainPassword);
    }

    /// <summary>
    /// ハッシュ化されたパスワードからPasswordを初期化します（内部使用）
    /// </summary>
    /// <param name="hashedValue">ハッシュ化されたパスワード</param>
    /// <param name="isFromHash">内部識別フラグ</param>
    private Password(string hashedValue, bool isFromHash)
    {
        HashedValue = hashedValue;
    }

    /// <summary>
    /// ハッシュ化されたパスワードからPasswordインスタンスを作成します
    /// </summary>
    /// <param name="hashedValue">ハッシュ化されたパスワード</param>
    /// <returns>Passwordインスタンス</returns>
    /// <exception cref="ArgumentException">無効なハッシュの場合</exception>
    public static Password FromHash(string hashedValue)
    {
        if (string.IsNullOrWhiteSpace(hashedValue))
            throw new ArgumentException("ハッシュ化されたパスワードは必須です。", nameof(hashedValue));

        if (!IsValidBCryptHash(hashedValue))
            throw new ArgumentException("無効なBCryptハッシュ形式です。", nameof(hashedValue));

        return new Password(hashedValue, true);
    }

    /// <summary>
    /// 平文パスワードが正しいかを検証します
    /// </summary>
    /// <param name="plainPassword">検証する平文パスワード</param>
    /// <returns>パスワードが正しい場合true</returns>
    public bool Verify(string plainPassword)
    {
        if (string.IsNullOrEmpty(plainPassword))
            return false;

        try
        {
            return BCrypt.Net.BCrypt.Verify(plainPassword, HashedValue);
        }
        catch (ArgumentException)
        {
            return false;
        }
        catch (FormatException)
        {
            return false;
        }
    }

    /// <summary>
    /// 文字列表現を返します（セキュリティのためマスク）
    /// </summary>
    /// <returns>マスクされた文字列</returns>
    public override string ToString() => "********";

    /// <summary>
    /// 平文パスワードのバリデーション
    /// </summary>
    /// <param name="plainPassword">検証する平文パスワード</param>
    /// <exception cref="ArgumentException">無効な値の場合</exception>
    private static void ValidatePlainPassword(string plainPassword)
    {
        if (string.IsNullOrWhiteSpace(plainPassword))
            throw new ArgumentException("パスワードは必須です。", nameof(plainPassword));

        if (plainPassword.Length < MinLength)
            throw new ArgumentException($"パスワードは{MinLength}文字以上で入力してください。", nameof(plainPassword));

        if (plainPassword.Length > MaxLength)
            throw new ArgumentException($"パスワードは{MaxLength}文字以内で入力してください。", nameof(plainPassword));
    }

    /// <summary>
    /// BCryptハッシュ形式の検証
    /// </summary>
    /// <param name="hash">検証するハッシュ</param>
    /// <returns>有効なBCryptハッシュの場合true</returns>
    private static bool IsValidBCryptHash(string hash)
    {
        // BCryptハッシュは$2a$、$2b$、$2x$、$2y$で始まる
        return hash.StartsWith("$2a$") ||
               hash.StartsWith("$2b$") ||
               hash.StartsWith("$2x$") ||
               hash.StartsWith("$2y$");
    }
}
