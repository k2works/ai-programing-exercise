using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.DependencyInjection.Extensions;
using MRS.Application.Ports;
using MRS.Application.DTOs.Auth;
using MRS.Infrastructure.Data;
using System.Data;
using Microsoft.Data.Sqlite;
using Microsoft.AspNetCore.Authentication;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Options;
using System.Security.Claims;
using System.Text.Encodings.Web;

namespace MRS.Api.Tests
{
    /// <summary>
    /// テスト用のWebApplicationFactory
    /// </summary>
    public class TestWebApplicationFactory : WebApplicationFactory<Program>
    {
        protected override void ConfigureWebHost(IWebHostBuilder builder)
        {
            builder.ConfigureServices(services =>
            {
                // テスト用のインメモリデータベース設定
                services.RemoveAll(typeof(IDbConnectionFactory));
                services.AddSingleton<IDbConnectionFactory, TestDbConnectionFactory>();

                // テスト用の認証サービス設定
                services.RemoveAll(typeof(IAuthService));
                services.AddSingleton<IAuthService, TestAuthService>();

                // テスト用のJWTトークンサービス設定
                services.RemoveAll(typeof(IJwtTokenService));
                services.AddSingleton<IJwtTokenService, TestJwtTokenService>();

                // テスト用の認証設定
                services.AddAuthentication("Test")
                    .AddScheme<TestAuthenticationSchemeOptions, TestAuthenticationHandler>("Test", options => { });

                services.AddAuthorization();
            });

            builder.UseEnvironment("Test");
        }
    }

    /// <summary>
    /// テスト用のデータベース接続ファクトリ
    /// </summary>
    public class TestDbConnectionFactory : IDbConnectionFactory
    {
        private readonly string _connectionString;
        private static int _instanceCounter = 0;
        private readonly object _lock = new object();
        private bool _initialized = false;

        public TestDbConnectionFactory()
        {
            var instanceId = Interlocked.Increment(ref _instanceCounter);
            var tempPath = Path.GetTempPath();
            _connectionString = $"Data Source={tempPath}test_{instanceId}_{Guid.NewGuid():N}.db";
        }

        public IDbConnection CreateConnection()
        {
            var connection = new SqliteConnection(_connectionString);
            connection.Open();
            
            lock (_lock)
            {
                if (!_initialized)
                {
                    // テスト用のテーブルを作成
                    CreateTables(connection);
                    SeedTestData(connection);
                    _initialized = true;
                }
            }
            
            return connection;
        }

        private void CreateTables(IDbConnection connection)
        {
            var createTablesScript = @"
                CREATE TABLE Users (
                    UserId VARCHAR(50) NOT NULL PRIMARY KEY,
                    Name VARCHAR(100) NOT NULL UNIQUE,
                    HashedPassword VARCHAR(255) NOT NULL,
                    Role VARCHAR(50) NOT NULL,
                    IsActive BOOLEAN NOT NULL DEFAULT true,
                    CreatedAt TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
                    UpdatedAt TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
                );

                CREATE TABLE Rooms (
                    RoomId TEXT PRIMARY KEY,
                    Name TEXT NOT NULL,
                    Capacity INTEGER NOT NULL,
                    Equipment TEXT,
                    CreatedAt TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP
                );

                CREATE TABLE Reservations (
                    ReservationId VARCHAR(50) NOT NULL PRIMARY KEY,
                    RoomId VARCHAR(50) NOT NULL,
                    UserId VARCHAR(50) NOT NULL,
                    Title VARCHAR(200) NOT NULL,
                    StartTime TIMESTAMP NOT NULL,
                    EndTime TIMESTAMP NOT NULL,
                    Participants TEXT,
                    Status VARCHAR(20) DEFAULT 'confirmed',
                    RowVersion INTEGER DEFAULT 0,
                    CreatedAt TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
                    UpdatedAt TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
                    FOREIGN KEY (RoomId) REFERENCES Rooms (RoomId),
                    FOREIGN KEY (UserId) REFERENCES Users (UserId)
                );

                CREATE TABLE AuditLogs (
                    Id INTEGER PRIMARY KEY AUTOINCREMENT,
                    EntityType TEXT NOT NULL,
                    EntityId TEXT NOT NULL,
                    Action TEXT NOT NULL,
                    UserId TEXT NOT NULL,
                    Timestamp TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
                    Changes TEXT,
                    IpAddress TEXT,
                    UserAgent TEXT
                );";

            using var command = connection.CreateCommand();
            command.CommandText = createTablesScript;
            command.ExecuteNonQuery();
        }

        private void SeedTestData(IDbConnection connection)
        {
            var seedDataScript = @"
                INSERT INTO Users (UserId, Name, HashedPassword, Role, IsActive) VALUES 
                ('admin01', 'admin01', '$2a$11$hashedpassword', 'Admin', 1),
                ('user01', 'user01', '$2a$11$hashedpassword', 'User', 1),
                ('user02', 'user02', '$2a$11$hashedpassword', 'User', 1);

                INSERT INTO Rooms (RoomId, Name, Capacity, Equipment) VALUES 
                ('room001', '会議室A', 10, 'プロジェクター,ホワイトボード'),
                ('room002', '会議室B', 8, 'ホワイトボード'),
                ('room003', '会議室C', 6, 'プロジェクター');

                INSERT INTO Reservations (ReservationId, RoomId, UserId, Title, StartTime, EndTime, Participants, Status, RowVersion) VALUES 
                ('test-reservation-001', 'room001', 'user01', 'テスト会議', '2024-01-15 10:00:00', '2024-01-15 12:00:00', '[]', 'confirmed', 1),
                ('user-reservation', 'room002', 'user02', 'プロジェクト会議', '2024-01-16 14:00:00', '2024-01-16 16:00:00', '[]', 'confirmed', 1),
                ('other-user-reservation', 'room003', 'user02', 'チーム会議', '2024-01-17 09:00:00', '2024-01-17 11:00:00', '[]', 'confirmed', 1);";

            using var command = connection.CreateCommand();
            command.CommandText = seedDataScript;
            command.ExecuteNonQuery();
        }
    }

    /// <summary>
    /// テスト用の認証サービス
    /// </summary>
    public class TestAuthService : IAuthService
    {
        public async Task<LoginResponseDto> LoginAsync(LoginRequestDto request, CancellationToken cancellationToken = default)
        {
            await Task.Delay(1, cancellationToken);

            // テスト用の固定認証
            if ((request.UserId == "admin01" || request.UserId == "user01" || request.UserId == "user02") && request.Password == "testpassword")
            {
                var isAdmin = request.UserId == "admin01";
                return new LoginResponseDto
                {
                    AccessToken = $"test-token-{request.UserId}",
                    RefreshToken = $"refresh-token-{request.UserId}",
                    ExpiresAt = DateTime.UtcNow.AddHours(1),
                    UserInfo = new UserInfoDto
                    {
                        UserId = request.UserId,
                        Name = request.UserId,
                        Role = isAdmin ? "Admin" : "User"
                    }
                };
            }

            throw new UnauthorizedAccessException("Invalid credentials");
        }

        public async Task<LoginResponseDto> RefreshTokenAsync(RefreshTokenRequestDto request, CancellationToken cancellationToken = default)
        {
            await Task.Delay(1, cancellationToken);
            
            if (request.RefreshToken.StartsWith("refresh-token-"))
            {
                var userId = request.RefreshToken.Replace("refresh-token-", "");
                var isAdmin = userId == "admin01";
                
                return new LoginResponseDto
                {
                    AccessToken = $"test-token-{userId}",
                    RefreshToken = $"refresh-token-{userId}",
                    ExpiresAt = DateTime.UtcNow.AddHours(1),
                    UserInfo = new UserInfoDto
                    {
                        UserId = userId,
                        Name = userId,
                        Role = isAdmin ? "Admin" : "User"
                    }
                };
            }
            
            throw new UnauthorizedAccessException("Invalid refresh token");
        }

        public async Task LogoutAsync(LogoutRequestDto request, CancellationToken cancellationToken = default)
        {
            await Task.Delay(1, cancellationToken);
            // テスト環境では何もしない
        }

        public async Task<UserInfoDto> ValidateTokenAsync(string token, CancellationToken cancellationToken = default)
        {
            await Task.Delay(1, cancellationToken);

            // テスト用のトークン検証
            if (token.StartsWith("test-token-"))
            {
                var userId = token.Replace("test-token-", "");
                var isAdmin = userId == "admin01";
                
                return new UserInfoDto
                {
                    UserId = userId,
                    Name = userId,
                    Role = isAdmin ? "Admin" : "User"
                };
            }

            throw new UnauthorizedAccessException("Invalid token");
        }
    }

    /// <summary>
    /// テスト用のJWTトークンサービス
    /// </summary>
    public class TestJwtTokenService : IJwtTokenService
    {
        public string GenerateAccessToken(UserInfoDto userInfo)
        {
            return $"test-token-{userInfo.UserId}";
        }

        public string GenerateRefreshToken()
        {
            return $"refresh-token-{Guid.NewGuid()}";
        }

        public UserInfoDto? ValidateAccessToken(string accessToken)
        {
            if (accessToken.StartsWith("test-token-"))
            {
                var userId = accessToken.Replace("test-token-", "");
                var isAdmin = userId == "admin01";
                
                return new UserInfoDto
                {
                    UserId = userId,
                    Name = userId,
                    Role = isAdmin ? "Admin" : "User"
                };
            }

            return null;
        }

        public bool ValidateRefreshToken(string refreshToken)
        {
            return refreshToken.StartsWith("refresh-token-");
        }

        public Task InvalidateRefreshTokenAsync(string refreshToken)
        {
            // テスト環境では何もしない
            return Task.CompletedTask;
        }
    }

    /// <summary>
    /// テスト用認証スキームオプション
    /// </summary>
    public class TestAuthenticationSchemeOptions : AuthenticationSchemeOptions { }

    /// <summary>
    /// テスト用認証ハンドラー
    /// </summary>
    public class TestAuthenticationHandler : AuthenticationHandler<TestAuthenticationSchemeOptions>
    {
        public TestAuthenticationHandler(IOptionsMonitor<TestAuthenticationSchemeOptions> options, ILoggerFactory logger, UrlEncoder encoder)
            : base(options, logger, encoder)
        {
        }

        protected override Task<AuthenticateResult> HandleAuthenticateAsync()
        {
            var authHeader = Request.Headers.Authorization.FirstOrDefault();
            
            if (string.IsNullOrEmpty(authHeader))
            {
                // 認証ヘッダーがない場合は認証なしで続行
                return Task.FromResult(AuthenticateResult.NoResult());
            }

            if (!authHeader.StartsWith("Bearer ", StringComparison.OrdinalIgnoreCase))
            {
                return Task.FromResult(AuthenticateResult.NoResult());
            }

            var token = authHeader[7..]; // "Bearer " を除去
            
            if (token.StartsWith("test-token-"))
            {
                var userId = token.Replace("test-token-", "");
                var isAdmin = userId == "admin01";

                var claims = new[]
                {
                    new Claim(ClaimTypes.Name, userId),
                    new Claim(ClaimTypes.NameIdentifier, userId),
                    new Claim(ClaimTypes.Role, isAdmin ? "Admin" : "User")
                };

                var identity = new ClaimsIdentity(claims, "Test");
                var principal = new ClaimsPrincipal(identity);

                return Task.FromResult(AuthenticateResult.Success(new AuthenticationTicket(principal, "Test")));
            }

            return Task.FromResult(AuthenticateResult.Fail("Invalid token"));
        }
    }
}