using System.Net;
using System.Net.Http.Json;
using System.Text.Json;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.Extensions.DependencyInjection;
using Moq;
using MRS.Application.Ports;
using MRS.Application.DTOs.Auth;

namespace MRS.Api.Tests.Controllers;

public class AuthControllerTests : IClassFixture<WebApplicationFactory<Program>>
{
    private readonly WebApplicationFactory<Program> _factory;
    private readonly HttpClient _client;
    private readonly Mock<IAuthService> _mockAuthService;
    private readonly JsonSerializerOptions _jsonOptions;

    public AuthControllerTests(WebApplicationFactory<Program> factory)
    {
        _mockAuthService = new Mock<IAuthService>();
        
        _jsonOptions = new JsonSerializerOptions
        {
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase
        };
        
        _factory = factory.WithWebHostBuilder(builder =>
        {
            builder.ConfigureServices(services =>
            {
                // Remove existing IAuthService registration
                var descriptor = services.SingleOrDefault(
                    d => d.ServiceType == typeof(IAuthService));
                if (descriptor != null)
                {
                    services.Remove(descriptor);
                }

                // Add mock IAuthService
                services.AddSingleton(_mockAuthService.Object);
            });
        });
        
        _client = _factory.CreateClient();
    }

    [Fact]
    public async Task Login_WithValidCredentials_ReturnsSuccessWithToken()
    {
        // Arrange
        var loginRequest = new LoginRequestDto
        {
            UserId = "test@example.com",
            Password = "password123"
        };
        var expectedResponse = new LoginResponseDto
        {
            AccessToken = "mock-access-token",
            RefreshToken = "mock-refresh-token",
            ExpiresAt = DateTime.UtcNow.AddSeconds(3600),
            UserInfo = new UserInfoDto
            {
                UserId = "test@example.com",
                Name = "Test User",
                Role = "User"
            }
        };

        _mockAuthService
            .Setup(x => x.LoginAsync(loginRequest, CancellationToken.None))
            .ReturnsAsync(expectedResponse);

        // Act
        var response = await _client.PostAsJsonAsync("/api/auth/login", loginRequest);

        // Assert
        Assert.Equal(HttpStatusCode.OK, response.StatusCode);
        
        var content = await response.Content.ReadAsStringAsync();
        var actualResponse = JsonSerializer.Deserialize<LoginResponseDto>(content, _jsonOptions);

        Assert.NotNull(actualResponse);
        Assert.Equal(expectedResponse.AccessToken, actualResponse.AccessToken);
        Assert.Equal(expectedResponse.RefreshToken, actualResponse.RefreshToken);
        Assert.True(Math.Abs((expectedResponse.ExpiresAt - actualResponse.ExpiresAt).TotalSeconds) < 10);
    }

    [Fact]
    public async Task Login_WithInvalidCredentials_ReturnsUnauthorized()
    {
        // Arrange
        var loginRequest = new LoginRequestDto
        {
            UserId = "invalid@example.com",
            Password = "wrongpassword"
        };

        _mockAuthService
            .Setup(x => x.LoginAsync(loginRequest, CancellationToken.None))
            .ThrowsAsync(new UnauthorizedAccessException("Invalid credentials"));

        // Act
        var response = await _client.PostAsJsonAsync("/api/auth/login", loginRequest);

        // Assert
        Assert.Equal(HttpStatusCode.Unauthorized, response.StatusCode);
    }

    [Fact]
    public async Task RefreshToken_WithValidToken_ReturnsNewTokens()
    {
        // Arrange
        var refreshRequest = new RefreshTokenRequestDto
        {
            RefreshToken = "valid-refresh-token"
        };
        var expectedResponse = new LoginResponseDto
        {
            AccessToken = "new-access-token",
            RefreshToken = "new-refresh-token",
            ExpiresAt = DateTime.UtcNow.AddSeconds(3600),
            UserInfo = new UserInfoDto
            {
                UserId = "test@example.com",
                Name = "Test User",
                Role = "User"
            }
        };

        _mockAuthService
            .Setup(x => x.RefreshTokenAsync(refreshRequest, CancellationToken.None))
            .ReturnsAsync(expectedResponse);

        // Act
        var response = await _client.PostAsJsonAsync("/api/auth/refresh", refreshRequest);

        // Assert
        Assert.Equal(HttpStatusCode.OK, response.StatusCode);
        
        var content = await response.Content.ReadAsStringAsync();
        var actualResponse = JsonSerializer.Deserialize<LoginResponseDto>(content, _jsonOptions);

        Assert.NotNull(actualResponse);
        Assert.Equal(expectedResponse.AccessToken, actualResponse.AccessToken);
        Assert.Equal(expectedResponse.RefreshToken, actualResponse.RefreshToken);
    }

    [Fact]
    public async Task Logout_WithValidToken_ReturnsNoContent()
    {
        // Arrange
        var logoutRequest = new LogoutRequestDto
        {
            RefreshToken = "valid-refresh-token"
        };

        _mockAuthService
            .Setup(x => x.LogoutAsync(logoutRequest, CancellationToken.None))
            .Returns(Task.CompletedTask);

        // Act
        var response = await _client.PostAsJsonAsync("/api/auth/logout", logoutRequest);

        // Assert
        Assert.Equal(HttpStatusCode.NoContent, response.StatusCode);
    }
}
