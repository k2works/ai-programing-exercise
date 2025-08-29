using MRS.Domain.ValueObjects;

namespace MRS.Domain.Entities;

/// <summary>
/// ユーザーエンティティ
/// </summary>
public class User : IEquatable<User>
{
    /// <summary>
    /// ユーザーID
    /// </summary>
    public UserId UserId { get; }

    /// <summary>
    /// ユーザー名
    /// </summary>
    public Name Name { get; private set; }

    /// <summary>
    /// パスワード
    /// </summary>
    public Password Password { get; private set; }

    /// <summary>
    /// ユーザー役割
    /// </summary>
    public UserRole Role { get; private set; }

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
    /// Userエンティティを初期化します
    /// </summary>
    /// <param name="userId">ユーザーID</param>
    /// <param name="name">ユーザー名</param>
    /// <param name="password">パスワード</param>
    /// <param name="role">ユーザー役割（デフォルト：Member）</param>
    /// <exception cref="ArgumentNullException">必須パラメータがnullの場合</exception>
    public User(UserId userId, Name name, Password password, UserRole role = UserRole.Member)
    {
        UserId = userId ?? throw new ArgumentNullException(nameof(userId));
        Name = name ?? throw new ArgumentNullException(nameof(name));
        Password = password ?? throw new ArgumentNullException(nameof(password));
        Role = role;
        IsActive = true;
        CreatedAt = DateTime.UtcNow;
        UpdatedAt = DateTime.UtcNow;
    }

    /// <summary>
    /// パスワードを変更します
    /// </summary>
    /// <param name="newPassword">新しいパスワード</param>
    /// <exception cref="ArgumentNullException">パスワードがnullの場合</exception>
    public void ChangePassword(Password newPassword)
    {
        Password = newPassword ?? throw new ArgumentNullException(nameof(newPassword));
        UpdatedAt = DateTime.UtcNow;
    }

    /// <summary>
    /// ユーザー名を変更します
    /// </summary>
    /// <param name="newName">新しいユーザー名</param>
    /// <exception cref="ArgumentNullException">ユーザー名がnullの場合</exception>
    public void ChangeName(Name newName)
    {
        Name = newName ?? throw new ArgumentNullException(nameof(newName));
        UpdatedAt = DateTime.UtcNow;
    }

    /// <summary>
    /// パスワードを検証します
    /// </summary>
    /// <param name="plainPassword">検証する平文パスワード</param>
    /// <returns>パスワードが正しい場合true</returns>
    public bool VerifyPassword(string plainPassword)
    {
        if (string.IsNullOrEmpty(plainPassword))
            return false;

        return Password.Verify(plainPassword);
    }

    /// <summary>
    /// ユーザーを非アクティブ化します
    /// </summary>
    public void Deactivate()
    {
        IsActive = false;
        UpdatedAt = DateTime.UtcNow;
    }

    /// <summary>
    /// ユーザーをアクティブ化します
    /// </summary>
    public void Activate()
    {
        IsActive = true;
        UpdatedAt = DateTime.UtcNow;
    }

    /// <summary>
    /// ユーザー役割を変更します
    /// </summary>
    /// <param name="newRole">新しい役割</param>
    public void ChangeRole(UserRole newRole)
    {
        Role = newRole;
        UpdatedAt = DateTime.UtcNow;
    }

    /// <summary>
    /// 等価性の比較（UserIdベース）
    /// </summary>
    /// <param name="other">比較対象のUser</param>
    /// <returns>UserIdが同じ場合true</returns>
    public bool Equals(User? other)
    {
        if (other is null) return false;
        if (ReferenceEquals(this, other)) return true;
        return UserId.Equals(other.UserId);
    }

    /// <summary>
    /// 等価性の比較
    /// </summary>
    /// <param name="obj">比較対象のオブジェクト</param>
    /// <returns>同じUserの場合true</returns>
    public override bool Equals(object? obj)
    {
        return Equals(obj as User);
    }

    /// <summary>
    /// ハッシュコードを取得します
    /// </summary>
    /// <returns>UserIdのハッシュコード</returns>
    public override int GetHashCode()
    {
        return UserId.GetHashCode();
    }

    /// <summary>
    /// 等価演算子
    /// </summary>
    public static bool operator ==(User? left, User? right)
    {
        return EqualityComparer<User>.Default.Equals(left, right);
    }

    /// <summary>
    /// 非等価演算子
    /// </summary>
    public static bool operator !=(User? left, User? right)
    {
        return !(left == right);
    }

    /// <summary>
    /// 文字列表現を返します（セキュリティのためパスワードは除外）
    /// </summary>
    /// <returns>ユーザー情報の文字列表現</returns>
    public override string ToString()
    {
        return $"User {{ UserId: {UserId}, Name: {Name}, Role: {Role}, IsActive: {IsActive} }}";
    }
}
