using System.Text;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using Moq;
using MRS.Api.Middleware;
using MRS.Application.Ports;
using MRS.Application.DTOs.Auth;
using Xunit;

namespace MRS.Api.Tests.Middleware;

public class JwtMiddlewareTests
{
    private readonly Mock<IAuthService> _authServiceMock;
    private readonly Mock<RequestDelegate> _nextMock;
    private readonly Mock<ILogger<JwtMiddleware>> _loggerMock;
    private readonly JwtMiddleware _middleware;

    public JwtMiddlewareTests()
    {
        _authServiceMock = new Mock<IAuthService>();
        _nextMock = new Mock<RequestDelegate>();
        _loggerMock = new Mock<ILogger<JwtMiddleware>>();
        _middleware = new JwtMiddleware(_nextMock.Object, _loggerMock.Object);
    }

    [Fact]
    public async Task InvokeAsync_WithValidToken_ShouldSetUserInContext()
    {
        // Arrange
        var context = CreateHttpContext();
        context.Request.Headers.Authorization = "Bearer valid-token";
        
        var userInfo = new UserInfoDto
        {
            UserId = "user-123",
            Name = "testuser",
            Role = "Member"
        };

        _authServiceMock
            .Setup(x => x.ValidateTokenAsync("valid-token", It.IsAny<CancellationToken>()))
            .ReturnsAsync(userInfo);

        context.RequestServices = CreateServiceProvider(_authServiceMock.Object);

        // Act
        await _middleware.InvokeAsync(context);

        // Assert
        Assert.NotNull(context.Items["User"]);
        var contextUser = context.Items["User"] as UserInfoDto;
        Assert.Equal("user-123", contextUser!.UserId);
        Assert.Equal("testuser", contextUser.Name);
        _nextMock.Verify(x => x(context), Times.Once);
    }

    [Fact]
    public async Task InvokeAsync_WithInvalidToken_ShouldReturn401()
    {
        // Arrange
        var context = CreateHttpContext();
        context.Request.Headers.Authorization = "Bearer invalid-token";

        _authServiceMock
            .Setup(x => x.ValidateTokenAsync("invalid-token", It.IsAny<CancellationToken>()))
            .ThrowsAsync(new UnauthorizedAccessException("Invalid token"));

        context.RequestServices = CreateServiceProvider(_authServiceMock.Object);

        // Act
        await _middleware.InvokeAsync(context);

        // Assert
        Assert.Equal(401, context.Response.StatusCode);
        Assert.Null(context.Items["User"]);
        _nextMock.Verify(x => x(context), Times.Never);
    }

    [Fact]
    public async Task InvokeAsync_WithoutToken_ShouldContinueWithoutAuth()
    {
        // Arrange
        var context = CreateHttpContext();
        // Authorization ヘッダーなし

        context.RequestServices = CreateServiceProvider(_authServiceMock.Object);

        // Act
        await _middleware.InvokeAsync(context);

        // Assert
        Assert.Null(context.Items["User"]);
        _nextMock.Verify(x => x(context), Times.Once);
        _authServiceMock.Verify(x => x.ValidateTokenAsync(It.IsAny<string>(), It.IsAny<CancellationToken>()), Times.Never);
    }

    [Fact]
    public async Task InvokeAsync_WithMalformedAuthHeader_ShouldContinueWithoutAuth()
    {
        // Arrange
        var context = CreateHttpContext();
        context.Request.Headers.Authorization = "InvalidFormat";

        context.RequestServices = CreateServiceProvider(_authServiceMock.Object);

        // Act
        await _middleware.InvokeAsync(context);

        // Assert
        Assert.Null(context.Items["User"]);
        _nextMock.Verify(x => x(context), Times.Once);
        _authServiceMock.Verify(x => x.ValidateTokenAsync(It.IsAny<string>(), It.IsAny<CancellationToken>()), Times.Never);
    }

    [Fact]
    public async Task InvokeAsync_AuthServiceThrowsException_ShouldReturn401()
    {
        // Arrange
        var context = CreateHttpContext();
        context.Request.Headers.Authorization = "Bearer token";

        _authServiceMock
            .Setup(x => x.ValidateTokenAsync("token", It.IsAny<CancellationToken>()))
            .ThrowsAsync(new Exception("Service error"));

        context.RequestServices = CreateServiceProvider(_authServiceMock.Object);

        // Act
        await _middleware.InvokeAsync(context);

        // Assert
        Assert.Equal(401, context.Response.StatusCode);
        Assert.Null(context.Items["User"]);
        _nextMock.Verify(x => x(context), Times.Never);
    }

    private static HttpContext CreateHttpContext()
    {
        var context = new DefaultHttpContext();
        context.Response.Body = new MemoryStream();
        return context;
    }

    private static IServiceProvider CreateServiceProvider(IAuthService authService)
    {
        var services = new ServiceCollection();
        services.AddSingleton(authService);
        return services.BuildServiceProvider();
    }
}
