using MRS.Application.DTOs.Auth;

namespace MRS.Application.Ports;

/// <summary>
/// 認証サービスのポート（入力）
/// </summary>
public interface IAuthService
{
    /// <summary>
    /// ログイン処理
    /// </summary>
    /// <param name="request">ログインリクエスト</param>
    /// <param name="cancellationToken">キャンセルトークン</param>
    /// <returns>ログインレスポンス（JWT含む）</returns>
    /// <exception cref="UnauthorizedAccessException">認証失敗時</exception>
    Task<LoginResponseDto> LoginAsync(LoginRequestDto request, CancellationToken cancellationToken = default);

    /// <summary>
    /// トークンリフレッシュ処理
    /// </summary>
    /// <param name="request">リフレッシュリクエスト</param>
    /// <param name="cancellationToken">キャンセルトークン</param>
    /// <returns>新しいトークン情報</returns>
    /// <exception cref="UnauthorizedAccessException">リフレッシュトークンが無効な場合</exception>
    Task<LoginResponseDto> RefreshTokenAsync(RefreshTokenRequestDto request, CancellationToken cancellationToken = default);

    /// <summary>
    /// ログアウト処理
    /// </summary>
    /// <param name="request">ログアウトリクエスト</param>
    /// <param name="cancellationToken">キャンセルトークン</param>
    /// <returns>完了タスク</returns>
    Task LogoutAsync(LogoutRequestDto request, CancellationToken cancellationToken = default);

    /// <summary>
    /// アクセストークンの検証
    /// </summary>
    /// <param name="accessToken">検証対象のアクセストークン</param>
    /// <param name="cancellationToken">キャンセルトークン</param>
    /// <returns>ユーザー情報</returns>
    /// <exception cref="UnauthorizedAccessException">トークンが無効な場合</exception>
    Task<UserInfoDto> ValidateTokenAsync(string accessToken, CancellationToken cancellationToken = default);
}
