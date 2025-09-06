using Microsoft.AspNetCore.Mvc;
using MRS.Application.Ports;
using MRS.Application.DTOs.Auth;
using MRS.Api.Services;

namespace MRS.Api.Controllers;

[ApiController]
[Route("api/[controller]")]
public class AuthController : ControllerBase
{
    private readonly IAuthService _authService;
    private readonly IMetricsService _metricsService;

    public AuthController(IAuthService authService, IMetricsService metricsService)
    {
        _authService = authService;
        _metricsService = metricsService;
    }

    /// <summary>
    /// ログイン
    /// </summary>
    [HttpPost("login")]
    public async Task<ActionResult<LoginResponseDto>> Login([FromBody] LoginRequestDto request)
    {
        try
        {
            var response = await _authService.LoginAsync(request);
            _metricsService.IncrementLoginSuccess();
            return Ok(response);
        }
        catch (UnauthorizedAccessException)
        {
            _metricsService.IncrementLoginFailure();
            return Unauthorized();
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
        await _authService.LogoutAsync(request);
        return NoContent();
    }
}
