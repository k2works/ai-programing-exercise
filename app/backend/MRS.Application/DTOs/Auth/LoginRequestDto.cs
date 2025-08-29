namespace MRS.Application.DTOs.Auth;

/// <summary>
/// ログインリクエストDTO
/// </summary>
public record LoginRequestDto
{
    /// <summary>
    /// ユーザーID
    /// </summary>
    public required string UserId { get; init; }

    /// <summary>
    /// パスワード
    /// </summary>
    public required string Password { get; init; }
}
