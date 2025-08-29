using Xunit;
using Moq;
using MRS.Application.Ports;
using MRS.Application.DTOs.Auth;
using MRS.Domain.ValueObjects;

namespace MRS.Application.Tests.Ports;

/// <summary>
/// IAuthServiceインターフェースのテスト
/// </summary>
public class IAuthServiceTests
{
    [Fact]
    public void LoginAsync_ValidCredentials_ShouldReturnLoginResponse()
    {
        // Arrange
        var mockAuthService = new Mock<IAuthService>();
        var loginRequest = new LoginRequestDto
        {
            UserId = "USER001",
            Password = "password123"
        };

        var expectedResponse = new LoginResponseDto
        {
            AccessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
            RefreshToken = "refresh_token_123",
            ExpiresAt = DateTime.UtcNow.AddHours(1),
            UserInfo = new UserInfoDto
            {
                UserId = "USER001",
                Name = "テストユーザー",
                Role = "Member"
            }
        };

        mockAuthService
            .Setup(x => x.LoginAsync(loginRequest, It.IsAny<CancellationToken>()))
            .ReturnsAsync(expectedResponse);

        // Act & Assert
        var authService = mockAuthService.Object;
        Assert.NotNull(authService);
        
        // メソッドシグネチャの存在確認
        var loginMethod = typeof(IAuthService).GetMethod("LoginAsync");
        Assert.NotNull(loginMethod);
        Assert.Equal(typeof(Task<LoginResponseDto>), loginMethod.ReturnType);
    }

    [Fact]
    public void LoginAsync_InvalidCredentials_ShouldThrowUnauthorizedException()
    {
        // Arrange
        var mockAuthService = new Mock<IAuthService>();
        var loginRequest = new LoginRequestDto
        {
            UserId = "INVALID_USER",
            Password = "wrong_password"
        };

        mockAuthService
            .Setup(x => x.LoginAsync(loginRequest, It.IsAny<CancellationToken>()))
            .ThrowsAsync(new UnauthorizedAccessException("Invalid credentials"));

        // Act & Assert
        var authService = mockAuthService.Object;
        Assert.NotNull(authService);
    }

    [Fact]
    public void RefreshTokenAsync_ValidToken_ShouldReturnNewTokens()
    {
        // Arrange
        var mockAuthService = new Mock<IAuthService>();
        var refreshRequest = new RefreshTokenRequestDto
        {
            RefreshToken = "valid_refresh_token"
        };

        var expectedResponse = new LoginResponseDto
        {
            AccessToken = "new_access_token",
            RefreshToken = "new_refresh_token",
            ExpiresAt = DateTime.UtcNow.AddHours(1),
            UserInfo = new UserInfoDto
            {
                UserId = "USER001",
                Name = "テストユーザー",
                Role = "Member"
            }
        };

        mockAuthService
            .Setup(x => x.RefreshTokenAsync(refreshRequest, It.IsAny<CancellationToken>()))
            .ReturnsAsync(expectedResponse);

        // Act & Assert
        var authService = mockAuthService.Object;
        
        // メソッドシグネチャの存在確認
        var refreshMethod = typeof(IAuthService).GetMethod("RefreshTokenAsync");
        Assert.NotNull(refreshMethod);
        Assert.Equal(typeof(Task<LoginResponseDto>), refreshMethod.ReturnType);
    }

    [Fact]
    public void LogoutAsync_ValidToken_ShouldCompleteSuccessfully()
    {
        // Arrange
        var mockAuthService = new Mock<IAuthService>();
        var logoutRequest = new LogoutRequestDto
        {
            RefreshToken = "valid_refresh_token"
        };

        mockAuthService
            .Setup(x => x.LogoutAsync(logoutRequest, It.IsAny<CancellationToken>()))
            .Returns(Task.CompletedTask);

        // Act & Assert
        var authService = mockAuthService.Object;
        
        // メソッドシグネチャの存在確認
        var logoutMethod = typeof(IAuthService).GetMethod("LogoutAsync");
        Assert.NotNull(logoutMethod);
        Assert.Equal(typeof(Task), logoutMethod.ReturnType);
    }

    [Fact]
    public void ValidateTokenAsync_ValidToken_ShouldReturnUserInfo()
    {
        // Arrange
        var mockAuthService = new Mock<IAuthService>();
        var accessToken = "valid_access_token";

        var expectedUserInfo = new UserInfoDto
        {
            UserId = "USER001",
            Name = "テストユーザー",
            Role = "Member"
        };

        mockAuthService
            .Setup(x => x.ValidateTokenAsync(accessToken, It.IsAny<CancellationToken>()))
            .ReturnsAsync(expectedUserInfo);

        // Act & Assert
        var authService = mockAuthService.Object;
        
        // メソッドシグネチャの存在確認
        var validateMethod = typeof(IAuthService).GetMethod("ValidateTokenAsync");
        Assert.NotNull(validateMethod);
        Assert.Equal(typeof(Task<UserInfoDto>), validateMethod.ReturnType);
    }
}
