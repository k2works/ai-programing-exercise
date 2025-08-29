namespace MRS.Application.DTOs.Auth;

/// <summary>
/// トークンリフレッシュリクエストDTO
/// </summary>
public record RefreshTokenRequestDto
{
    /// <summary>
    /// リフレッシュトークン
    /// </summary>
    public required string RefreshToken { get; init; }
}

/// <summary>
/// ログアウトリクエストDTO
/// </summary>
public record LogoutRequestDto
{
    /// <summary>
    /// リフレッシュトークン（無効化対象）
    /// </summary>
    public required string RefreshToken { get; init; }
}
