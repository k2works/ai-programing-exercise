using MRS.Application.DTOs.Auth;
using MRS.Application.Ports;
using MRS.Domain.Entities;
using MRS.Domain.ValueObjects;

namespace MRS.Application.Services;

/// <summary>
/// 認証サービス実装
/// </summary>
public class AuthService : IAuthService
{
    private readonly IUserRepository _userRepository;
    private readonly IJwtTokenService _jwtTokenService;

    public AuthService(IUserRepository userRepository, IJwtTokenService jwtTokenService)
    {
        _userRepository = userRepository ?? throw new ArgumentNullException(nameof(userRepository));
        _jwtTokenService = jwtTokenService ?? throw new ArgumentNullException(nameof(jwtTokenService));
    }

    /// <summary>
    /// ログイン処理
    /// </summary>
    public async Task<LoginResponseDto> LoginAsync(LoginRequestDto request, CancellationToken cancellationToken = default)
    {
        if (request == null)
            throw new ArgumentNullException(nameof(request));

        // ユーザー検索
        var userId = new UserId(request.UserId);
        var user = await _userRepository.GetByIdAsync(userId, cancellationToken);

        if (user == null)
            throw new UnauthorizedAccessException("Invalid credentials");

        // アクティブ状態チェック
        if (!user.IsActive)
            throw new UnauthorizedAccessException("User account is inactive");

        // パスワード検証
        if (!user.Password.Verify(request.Password))
            throw new UnauthorizedAccessException("Invalid credentials");

        // ユーザー情報DTO作成
        var userInfo = new UserInfoDto
        {
            UserId = user.UserId.Value,
            Name = user.Name.Value,
            Role = user.Role.ToString()
        };

        // トークン生成
        var accessToken = _jwtTokenService.GenerateAccessToken(userInfo);
        var refreshToken = _jwtTokenService.GenerateRefreshToken();

        return new LoginResponseDto
        {
            AccessToken = accessToken,
            RefreshToken = refreshToken,
            ExpiresAt = DateTime.UtcNow.AddHours(1), // 1時間後に期限切れ
            UserInfo = userInfo
        };
    }

    /// <summary>
    /// トークンリフレッシュ処理
    /// </summary>
    public async Task<LoginResponseDto> RefreshTokenAsync(RefreshTokenRequestDto request, CancellationToken cancellationToken = default)
    {
        if (request == null)
            throw new ArgumentNullException(nameof(request));

        // リフレッシュトークン検証
        if (!_jwtTokenService.ValidateRefreshToken(request.RefreshToken))
            throw new UnauthorizedAccessException("Invalid refresh token");

        // 簡略化：実際の実装では、リフレッシュトークンからユーザー情報を取得する必要がある
        // この段階では、固定のユーザー情報を使用
        var userInfo = new UserInfoDto
        {
            UserId = "USER001", // TODO: リフレッシュトークンから取得
            Name = "テストユーザー", // TODO: リフレッシュトークンから取得
            Role = "Member" // TODO: リフレッシュトークンから取得
        };

        // 新しいトークン生成
        var newAccessToken = _jwtTokenService.GenerateAccessToken(userInfo);
        var newRefreshToken = _jwtTokenService.GenerateRefreshToken();

        return new LoginResponseDto
        {
            AccessToken = newAccessToken,
            RefreshToken = newRefreshToken,
            ExpiresAt = DateTime.UtcNow.AddHours(1),
            UserInfo = userInfo
        };
    }

    /// <summary>
    /// ログアウト処理
    /// </summary>
    public async Task LogoutAsync(LogoutRequestDto request, CancellationToken cancellationToken = default)
    {
        if (request == null)
            throw new ArgumentNullException(nameof(request));

        // リフレッシュトークンを無効化
        await _jwtTokenService.InvalidateRefreshTokenAsync(request.RefreshToken);
    }

    /// <summary>
    /// アクセストークンの検証
    /// </summary>
    public async Task<UserInfoDto> ValidateTokenAsync(string accessToken, CancellationToken cancellationToken = default)
    {
        if (string.IsNullOrWhiteSpace(accessToken))
            throw new ArgumentException("Access token is required", nameof(accessToken));

        // アクセストークン検証
        var userInfo = _jwtTokenService.ValidateAccessToken(accessToken);

        if (userInfo == null)
            throw new UnauthorizedAccessException("Invalid access token");

        return await Task.FromResult(userInfo);
    }
}
