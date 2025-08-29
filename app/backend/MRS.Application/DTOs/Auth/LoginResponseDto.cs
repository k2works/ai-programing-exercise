namespace MRS.Application.DTOs.Auth;

/// <summary>
/// ログインレスポンスDTO
/// </summary>
public record LoginResponseDto
{
    /// <summary>
    /// JWTアクセストークン
    /// </summary>
    public required string AccessToken { get; init; }

    /// <summary>
    /// リフレッシュトークン
    /// </summary>
    public required string RefreshToken { get; init; }

    /// <summary>
    /// トークン有効期限（UTC）
    /// </summary>
    public required DateTime ExpiresAt { get; init; }

    /// <summary>
    /// ユーザー情報
    /// </summary>
    public required UserInfoDto UserInfo { get; init; }
}

/// <summary>
/// ユーザー情報DTO
/// </summary>
public record UserInfoDto
{
    /// <summary>
    /// ユーザーID
    /// </summary>
    public required string UserId { get; init; }

    /// <summary>
    /// ユーザー名
    /// </summary>
    public required string Name { get; init; }

    /// <summary>
    /// ユーザーロール
    /// </summary>
    public required string Role { get; init; }
}
