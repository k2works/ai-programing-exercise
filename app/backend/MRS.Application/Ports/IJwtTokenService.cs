using MRS.Application.DTOs.Auth;

namespace MRS.Application.Ports;

/// <summary>
/// JWT トークンサービスのポート（出力）
/// </summary>
public interface IJwtTokenService
{
    /// <summary>
    /// アクセストークンを生成
    /// </summary>
    /// <param name="userInfo">ユーザー情報</param>
    /// <returns>アクセストークン</returns>
    string GenerateAccessToken(UserInfoDto userInfo);

    /// <summary>
    /// リフレッシュトークンを生成
    /// </summary>
    /// <returns>リフレッシュトークン</returns>
    string GenerateRefreshToken();

    /// <summary>
    /// アクセストークンを検証
    /// </summary>
    /// <param name="accessToken">検証対象のアクセストークン</param>
    /// <returns>ユーザー情報（無効な場合はnull）</returns>
    UserInfoDto? ValidateAccessToken(string accessToken);

    /// <summary>
    /// リフレッシュトークンを検証
    /// </summary>
    /// <param name="refreshToken">検証対象のリフレッシュトークン</param>
    /// <returns>有効な場合true</returns>
    bool ValidateRefreshToken(string refreshToken);

    /// <summary>
    /// リフレッシュトークンを無効化
    /// </summary>
    /// <param name="refreshToken">無効化するリフレッシュトークン</param>
    /// <returns>完了タスク</returns>
    Task InvalidateRefreshTokenAsync(string refreshToken);
}
