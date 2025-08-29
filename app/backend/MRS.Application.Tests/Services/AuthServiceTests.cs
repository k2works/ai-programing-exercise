using Xunit;
using Moq;
using MRS.Application.Services;
using MRS.Application.Ports;
using MRS.Application.DTOs.Auth;
using MRS.Domain.Entities;
using MRS.Domain.ValueObjects;

namespace MRS.Application.Tests.Services;

/// <summary>
/// AuthServiceのテスト
/// </summary>
public class AuthServiceTests
{
    private readonly Mock<IUserRepository> _userRepositoryMock;
    private readonly Mock<IJwtTokenService> _jwtTokenServiceMock;
    private readonly AuthService _authService;

    public AuthServiceTests()
    {
        _userRepositoryMock = new Mock<IUserRepository>();
        _jwtTokenServiceMock = new Mock<IJwtTokenService>();
        _authService = new AuthService(_userRepositoryMock.Object, _jwtTokenServiceMock.Object);
    }

    [Fact]
    public async Task LoginAsync_ValidCredentials_ShouldReturnLoginResponse()
    {
        // Arrange
        var loginRequest = new LoginRequestDto
        {
            UserId = "USER001",
            Password = "password123"
        };

        var user = new User(
            new UserId("USER001"),
            new Name("テストユーザー"),
            new Password("password123"),
            UserRole.Member
        );

        var userInfo = new UserInfoDto
        {
            UserId = "USER001",
            Name = "テストユーザー",
            Role = "Member"
        };

        var accessToken = "access_token_123";
        var refreshToken = "refresh_token_123";

        _userRepositoryMock
            .Setup(x => x.GetByIdAsync(It.IsAny<UserId>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync(user);

        _jwtTokenServiceMock
            .Setup(x => x.GenerateAccessToken(It.IsAny<UserInfoDto>()))
            .Returns(accessToken);

        _jwtTokenServiceMock
            .Setup(x => x.GenerateRefreshToken())
            .Returns(refreshToken);

        // Act
        var result = await _authService.LoginAsync(loginRequest, CancellationToken.None);

        // Assert
        Assert.NotNull(result);
        Assert.Equal(accessToken, result.AccessToken);
        Assert.Equal(refreshToken, result.RefreshToken);
        Assert.Equal("USER001", result.UserInfo.UserId);
        Assert.Equal("テストユーザー", result.UserInfo.Name);
        Assert.Equal("Member", result.UserInfo.Role);
        Assert.True(result.ExpiresAt > DateTime.UtcNow);

        _userRepositoryMock.Verify(x => x.GetByIdAsync(It.IsAny<UserId>(), It.IsAny<CancellationToken>()), Times.Once);
        _jwtTokenServiceMock.Verify(x => x.GenerateAccessToken(It.IsAny<UserInfoDto>()), Times.Once);
        _jwtTokenServiceMock.Verify(x => x.GenerateRefreshToken(), Times.Once);
    }

    [Fact]
    public async Task LoginAsync_UserNotFound_ShouldThrowUnauthorizedAccessException()
    {
        // Arrange
        var loginRequest = new LoginRequestDto
        {
            UserId = "INVALID_USER",
            Password = "password123"
        };

        _userRepositoryMock
            .Setup(x => x.GetByIdAsync(It.IsAny<UserId>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync((User?)null);

        // Act & Assert
        await Assert.ThrowsAsync<UnauthorizedAccessException>(() => 
            _authService.LoginAsync(loginRequest, CancellationToken.None));

        _userRepositoryMock.Verify(x => x.GetByIdAsync(It.IsAny<UserId>(), It.IsAny<CancellationToken>()), Times.Once);
        _jwtTokenServiceMock.Verify(x => x.GenerateAccessToken(It.IsAny<UserInfoDto>()), Times.Never);
    }

    [Fact]
    public async Task LoginAsync_InvalidPassword_ShouldThrowUnauthorizedAccessException()
    {
        // Arrange
        var loginRequest = new LoginRequestDto
        {
            UserId = "USER001",
            Password = "wrong_password"
        };

        var user = new User(
            new UserId("USER001"),
            new Name("テストユーザー"),
            new Password("correct_password"),
            UserRole.Member
        );

        _userRepositoryMock
            .Setup(x => x.GetByIdAsync(It.IsAny<UserId>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync(user);

        // Act & Assert
        await Assert.ThrowsAsync<UnauthorizedAccessException>(() => 
            _authService.LoginAsync(loginRequest, CancellationToken.None));

        _userRepositoryMock.Verify(x => x.GetByIdAsync(It.IsAny<UserId>(), It.IsAny<CancellationToken>()), Times.Once);
        _jwtTokenServiceMock.Verify(x => x.GenerateAccessToken(It.IsAny<UserInfoDto>()), Times.Never);
    }

    [Fact]
    public async Task LoginAsync_InactiveUser_ShouldThrowUnauthorizedAccessException()
    {
        // Arrange
        var loginRequest = new LoginRequestDto
        {
            UserId = "USER001",
            Password = "password123"
        };

        var user = new User(
            new UserId("USER001"),
            new Name("テストユーザー"),
            new Password("password123"),
            UserRole.Member
        );
        user.Deactivate(); // ユーザーを非アクティブ化

        _userRepositoryMock
            .Setup(x => x.GetByIdAsync(It.IsAny<UserId>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync(user);

        // Act & Assert
        await Assert.ThrowsAsync<UnauthorizedAccessException>(() => 
            _authService.LoginAsync(loginRequest, CancellationToken.None));

        _userRepositoryMock.Verify(x => x.GetByIdAsync(It.IsAny<UserId>(), It.IsAny<CancellationToken>()), Times.Once);
        _jwtTokenServiceMock.Verify(x => x.GenerateAccessToken(It.IsAny<UserInfoDto>()), Times.Never);
    }

    [Fact]
    public async Task RefreshTokenAsync_ValidToken_ShouldReturnNewTokens()
    {
        // Arrange
        var refreshRequest = new RefreshTokenRequestDto
        {
            RefreshToken = "valid_refresh_token"
        };

        var userInfo = new UserInfoDto
        {
            UserId = "USER001",
            Name = "テストユーザー",
            Role = "Member"
        };

        var newAccessToken = "new_access_token";
        var newRefreshToken = "new_refresh_token";

        _jwtTokenServiceMock
            .Setup(x => x.ValidateRefreshToken(refreshRequest.RefreshToken))
            .Returns(true);

        // 簡略化のため、リフレッシュトークンから直接ユーザー情報を取得するモック
        _jwtTokenServiceMock
            .Setup(x => x.GenerateAccessToken(It.IsAny<UserInfoDto>()))
            .Returns(newAccessToken);

        _jwtTokenServiceMock
            .Setup(x => x.GenerateRefreshToken())
            .Returns(newRefreshToken);

        // Act
        var result = await _authService.RefreshTokenAsync(refreshRequest, CancellationToken.None);

        // Assert
        Assert.NotNull(result);
        Assert.Equal(newAccessToken, result.AccessToken);
        Assert.Equal(newRefreshToken, result.RefreshToken);
        Assert.True(result.ExpiresAt > DateTime.UtcNow);

        _jwtTokenServiceMock.Verify(x => x.ValidateRefreshToken(refreshRequest.RefreshToken), Times.Once);
        _jwtTokenServiceMock.Verify(x => x.GenerateAccessToken(It.IsAny<UserInfoDto>()), Times.Once);
        _jwtTokenServiceMock.Verify(x => x.GenerateRefreshToken(), Times.Once);
    }

    [Fact]
    public async Task RefreshTokenAsync_InvalidToken_ShouldThrowUnauthorizedAccessException()
    {
        // Arrange
        var refreshRequest = new RefreshTokenRequestDto
        {
            RefreshToken = "invalid_refresh_token"
        };

        _jwtTokenServiceMock
            .Setup(x => x.ValidateRefreshToken(refreshRequest.RefreshToken))
            .Returns(false);

        // Act & Assert
        await Assert.ThrowsAsync<UnauthorizedAccessException>(() => 
            _authService.RefreshTokenAsync(refreshRequest, CancellationToken.None));

        _jwtTokenServiceMock.Verify(x => x.ValidateRefreshToken(refreshRequest.RefreshToken), Times.Once);
        _jwtTokenServiceMock.Verify(x => x.GenerateAccessToken(It.IsAny<UserInfoDto>()), Times.Never);
    }

    [Fact]
    public async Task LogoutAsync_ValidToken_ShouldInvalidateToken()
    {
        // Arrange
        var logoutRequest = new LogoutRequestDto
        {
            RefreshToken = "valid_refresh_token"
        };

        _jwtTokenServiceMock
            .Setup(x => x.InvalidateRefreshTokenAsync(logoutRequest.RefreshToken))
            .Returns(Task.CompletedTask);

        // Act
        await _authService.LogoutAsync(logoutRequest, CancellationToken.None);

        // Assert
        _jwtTokenServiceMock.Verify(x => x.InvalidateRefreshTokenAsync(logoutRequest.RefreshToken), Times.Once);
    }

    [Fact]
    public async Task ValidateTokenAsync_ValidToken_ShouldReturnUserInfo()
    {
        // Arrange
        var accessToken = "valid_access_token";
        var expectedUserInfo = new UserInfoDto
        {
            UserId = "USER001",
            Name = "テストユーザー",
            Role = "Member"
        };

        _jwtTokenServiceMock
            .Setup(x => x.ValidateAccessToken(accessToken))
            .Returns(expectedUserInfo);

        // Act
        var result = await _authService.ValidateTokenAsync(accessToken, CancellationToken.None);

        // Assert
        Assert.NotNull(result);
        Assert.Equal(expectedUserInfo.UserId, result.UserId);
        Assert.Equal(expectedUserInfo.Name, result.Name);
        Assert.Equal(expectedUserInfo.Role, result.Role);

        _jwtTokenServiceMock.Verify(x => x.ValidateAccessToken(accessToken), Times.Once);
    }

    [Fact]
    public async Task ValidateTokenAsync_InvalidToken_ShouldThrowUnauthorizedAccessException()
    {
        // Arrange
        var accessToken = "invalid_access_token";

        _jwtTokenServiceMock
            .Setup(x => x.ValidateAccessToken(accessToken))
            .Returns((UserInfoDto?)null);

        // Act & Assert
        await Assert.ThrowsAsync<UnauthorizedAccessException>(() => 
            _authService.ValidateTokenAsync(accessToken, CancellationToken.None));

        _jwtTokenServiceMock.Verify(x => x.ValidateAccessToken(accessToken), Times.Once);
    }
}
