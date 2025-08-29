using System.IdentityModel.Tokens.Jwt;
using System.Security.Claims;
using System.Text;
using Microsoft.Extensions.Configuration;
using Microsoft.IdentityModel.Tokens;
using MRS.Application.Ports;
using MRS.Application.DTOs.Auth;

namespace MRS.Application.Services;

/// <summary>
/// JWT トークンサービス実装
/// </summary>
public class JwtTokenService : IJwtTokenService
{
    private readonly IConfiguration _configuration;
    private readonly string _key;
    private readonly string _issuer;
    private readonly string _audience;
    private readonly int _expirationMinutes;

    public JwtTokenService(IConfiguration configuration)
    {
        _configuration = configuration ?? throw new ArgumentNullException(nameof(configuration));
        _key = _configuration["Jwt:Key"] ?? throw new InvalidOperationException("JWT Key is not configured");
        _issuer = _configuration["Jwt:Issuer"] ?? "MRS.Api";
        _audience = _configuration["Jwt:Audience"] ?? "MRS.Client";
        _expirationMinutes = int.Parse(_configuration["Jwt:ExpirationMinutes"] ?? "30");
    }

    public string GenerateAccessToken(UserInfoDto userInfo)
    {
        if (userInfo == null)
            throw new ArgumentNullException(nameof(userInfo));

        var tokenHandler = new JwtSecurityTokenHandler();
        var keyBytes = Encoding.UTF8.GetBytes(_key);
        var tokenDescriptor = new SecurityTokenDescriptor
        {
            Subject = new ClaimsIdentity(new[]
            {
                new Claim(ClaimTypes.NameIdentifier, userInfo.UserId),
                new Claim(ClaimTypes.Name, userInfo.Name),
                new Claim(ClaimTypes.Role, userInfo.Role)
            }),
            Expires = DateTime.UtcNow.AddMinutes(_expirationMinutes),
            Issuer = _issuer,
            Audience = _audience,
            SigningCredentials = new SigningCredentials(
                new SymmetricSecurityKey(keyBytes),
                SecurityAlgorithms.HmacSha256Signature)
        };

        var token = tokenHandler.CreateToken(tokenDescriptor);
        return tokenHandler.WriteToken(token);
    }

    public UserInfoDto? ValidateAccessToken(string token)
    {
        if (string.IsNullOrWhiteSpace(token))
            return null;

        var tokenHandler = new JwtSecurityTokenHandler();
        var keyBytes = Encoding.UTF8.GetBytes(_key);

        try
        {
            tokenHandler.ValidateToken(token, new TokenValidationParameters
            {
                ValidateIssuerSigningKey = true,
                IssuerSigningKey = new SymmetricSecurityKey(keyBytes),
                ValidateIssuer = true,
                ValidIssuer = _issuer,
                ValidateAudience = true,
                ValidAudience = _audience,
                ValidateLifetime = true,
                ClockSkew = TimeSpan.Zero
            }, out SecurityToken validatedToken);

            var jwtToken = (JwtSecurityToken)validatedToken;
            var userId = jwtToken.Claims.First(x => x.Type == ClaimTypes.NameIdentifier).Value;
            var name = jwtToken.Claims.First(x => x.Type == ClaimTypes.Name).Value;
            var role = jwtToken.Claims.First(x => x.Type == ClaimTypes.Role).Value;

            return new UserInfoDto
            {
                UserId = userId,
                Name = name,
                Role = role
            };
        }
        catch
        {
            return null;
        }
    }

    public string GenerateRefreshToken()
    {
        var randomNumber = new byte[32];
        using var rng = System.Security.Cryptography.RandomNumberGenerator.Create();
        rng.GetBytes(randomNumber);
        return Convert.ToBase64String(randomNumber);
    }

    public bool ValidateRefreshToken(string refreshToken)
    {
        // Note: 実際の実装ではデータベースやキャッシュでの検証が必要
        // 現時点では簡易実装
        return !string.IsNullOrWhiteSpace(refreshToken) && refreshToken.Length > 20;
    }

    public Task InvalidateRefreshTokenAsync(string refreshToken)
    {
        // Note: 実際の実装ではデータベースやキャッシュからトークンを削除
        // 現時点では簡易実装
        return Task.CompletedTask;
    }
}