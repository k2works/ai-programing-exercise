using Microsoft.AspNetCore.Mvc;
using MRS.Application.Ports;
using MRS.Application.DTOs.Auth;
using MRS.Api.Services;
using Microsoft.AspNetCore.RateLimiting;

namespace MRS.Api.Controllers;

[ApiController]
[Route("api/[controller]")]
public class AuthController : ControllerBase
{
    private readonly IAuthService _authService;
    private readonly IMetricsService _metricsService;
    private readonly ISecurityLogService _securityLogService;

    public AuthController(IAuthService authService, IMetricsService metricsService, ISecurityLogService securityLogService)
    {
        _authService = authService;
        _metricsService = metricsService;
        _securityLogService = securityLogService;
    }

    /// <summary>
    /// ログイン
    /// </summary>
    [HttpPost("login")]
    [EnableRateLimiting("LoginPolicy")]
    public async Task<ActionResult<LoginResponseDto>> Login([FromBody] LoginRequestDto request)
    {
        var ipAddress = HttpContext.Connection.RemoteIpAddress?.ToString();
        var userAgent = HttpContext.Request.Headers.UserAgent.ToString();

        try
        {
            var response = await _authService.LoginAsync(request);
            _metricsService.IncrementLoginSuccess();
            _securityLogService.LogLoginAttempt(request.Username, true, ipAddress, userAgent);
            return Ok(response);
        }
        catch (UnauthorizedAccessException)
        {
            _metricsService.IncrementLoginFailure();
            _securityLogService.LogLoginAttempt(request.Username, false, ipAddress, userAgent);
            return Unauthorized(new { message = "Invalid username or password" });
        }
        catch (Exception ex)
        {
            _securityLogService.LogSecurityViolation("LoginError", ex.Message, request.Username, ipAddress);
            return StatusCode(500, new { message = "An error occurred during login" });
        }
    }

    /// <summary>
    /// トークンリフレッシュ
    /// </summary>
    [HttpPost("refresh")]
    public async Task<ActionResult<LoginResponseDto>> RefreshToken([FromBody] RefreshTokenRequestDto request)
    {
        try
        {
            var response = await _authService.RefreshTokenAsync(request);
            return Ok(response);
        }
        catch (UnauthorizedAccessException)
        {
            return Unauthorized();
        }
    }

    /// <summary>
    /// ログアウト
    /// </summary>
    [HttpPost("logout")]
    public async Task<IActionResult> Logout([FromBody] LogoutRequestDto request)
    {
        var ipAddress = HttpContext.Connection.RemoteIpAddress?.ToString();
        var username = HttpContext.User?.Identity?.Name;

        try
        {
            await _authService.LogoutAsync(request);
            if (!string.IsNullOrEmpty(username))
            {
                _securityLogService.LogLogout(username, ipAddress);
            }
            return NoContent();
        }
        catch (Exception ex)
        {
            _securityLogService.LogSecurityViolation("LogoutError", ex.Message, username, ipAddress);
            return StatusCode(500, new { message = "An error occurred during logout" });
        }
    }
}
