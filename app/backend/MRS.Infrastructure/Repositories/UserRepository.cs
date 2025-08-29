using System.Data;
using BCrypt.Net;
using Dapper;
using MRS.Application.Ports;
using MRS.Domain.Entities;
using MRS.Domain.ValueObjects;
using MRS.Infrastructure.Data;

namespace MRS.Infrastructure.Repositories;

/// <summary>
/// ユーザーリポジトリの実装（Dapper版）
/// </summary>
public class UserRepository : IUserRepository
{
    private readonly IDbConnectionFactory _connectionFactory;

    public UserRepository(IDbConnectionFactory connectionFactory)
    {
        _connectionFactory = connectionFactory ?? throw new ArgumentNullException(nameof(connectionFactory));
        InitializeDatabaseAsync().GetAwaiter().GetResult();
    }

    /// <summary>
    /// データベース初期化（テーブル作成とサンプルデータ挿入）
    /// </summary>
    private async Task InitializeDatabaseAsync()
    {
        using var connection = _connectionFactory.CreateConnection();
        
        // SQLiteでFOREIGN KEY制約を有効化
        await connection.ExecuteAsync("PRAGMA foreign_keys = ON");
        
        const string createTableSql = @"
            CREATE TABLE IF NOT EXISTS Users (
                UserId VARCHAR(50) NOT NULL PRIMARY KEY,
                Name VARCHAR(100) NOT NULL UNIQUE,
                HashedPassword VARCHAR(255) NOT NULL,
                Role VARCHAR(50) NOT NULL,
                IsActive BOOLEAN NOT NULL DEFAULT true,
                CreatedAt TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
                UpdatedAt TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
            );

            CREATE INDEX IF NOT EXISTS idx_users_name ON Users(Name);
            ";

        await connection.ExecuteAsync(createTableSql);

        // サンプルデータ挿入（BCryptハッシュを使用）
        var hashedPassword = BCrypt.Net.BCrypt.HashPassword("password123");
        
        const string insertUserSql = @"
            INSERT OR REPLACE INTO Users (UserId, Name, HashedPassword, Role, IsActive, CreatedAt, UpdatedAt) 
            VALUES (@UserId, @Name, @HashedPassword, @Role, @IsActive, @CreatedAt, @UpdatedAt)";

        await connection.ExecuteAsync(insertUserSql, new[]
        {
            new { UserId = "admin01", Name = "管理者", HashedPassword = hashedPassword, Role = "Admin", IsActive = true, CreatedAt = DateTime.Now, UpdatedAt = DateTime.Now },
            new { UserId = "user01", Name = "田中太郎", HashedPassword = hashedPassword, Role = "Member", IsActive = true, CreatedAt = DateTime.Now, UpdatedAt = DateTime.Now },
            new { UserId = "user02", Name = "佐藤花子", HashedPassword = hashedPassword, Role = "Member", IsActive = true, CreatedAt = DateTime.Now, UpdatedAt = DateTime.Now }
        });
    }

    /// <summary>
    /// ユーザーIDでユーザーを取得
    /// </summary>
    public async Task<User?> GetByIdAsync(UserId userId, CancellationToken cancellationToken = default)
    {
        const string sql = @"
            SELECT UserId, Name, HashedPassword, Role, IsActive, CreatedAt, UpdatedAt 
            FROM Users 
            WHERE UserId = @UserId";

        using var connection = _connectionFactory.CreateConnection();
        var userRow = await connection.QueryFirstOrDefaultAsync<UserRow>(sql, new { UserId = userId.Value });

        return userRow?.ToEntity();
    }

    /// <summary>
    /// ユーザー名でユーザーを取得
    /// </summary>
    public async Task<User?> GetByNameAsync(Name userName, CancellationToken cancellationToken = default)
    {
        const string sql = @"
            SELECT UserId, Name, HashedPassword, Role, IsActive, CreatedAt, UpdatedAt 
            FROM Users 
            WHERE Name = @Name";

        using var connection = _connectionFactory.CreateConnection();
        var userRow = await connection.QueryFirstOrDefaultAsync<UserRow>(sql, new { Name = userName.Value });

        return userRow?.ToEntity();
    }

    /// <summary>
    /// すべてのユーザーを取得
    /// </summary>
    public async Task<IEnumerable<User>> GetAllAsync(CancellationToken cancellationToken = default)
    {
        const string sql = @"
            SELECT UserId, Name, HashedPassword, Role, IsActive, CreatedAt, UpdatedAt 
            FROM Users 
            ORDER BY CreatedAt";

        using var connection = _connectionFactory.CreateConnection();
        var userRows = await connection.QueryAsync<UserRow>(sql);

        return userRows.Select(row => row.ToEntity()).ToList();
    }

    /// <summary>
    /// ユーザーを追加
    /// </summary>
    public async Task AddAsync(User user, CancellationToken cancellationToken = default)
    {
        const string sql = @"
            INSERT INTO Users (UserId, Name, HashedPassword, Role, IsActive, CreatedAt, UpdatedAt)
            VALUES (@UserId, @Name, @HashedPassword, @Role, @IsActive, @CreatedAt, @UpdatedAt)";

        using var connection = _connectionFactory.CreateConnection();
        await connection.ExecuteAsync(sql, UserRow.FromEntity(user));
    }

    /// <summary>
    /// ユーザーを更新
    /// </summary>
    public async Task UpdateAsync(User user, CancellationToken cancellationToken = default)
    {
        const string sql = @"
            UPDATE Users 
            SET Name = @Name, 
                HashedPassword = @HashedPassword, 
                Role = @Role, 
                IsActive = @IsActive, 
                UpdatedAt = @UpdatedAt
            WHERE UserId = @UserId";

        using var connection = _connectionFactory.CreateConnection();
        await connection.ExecuteAsync(sql, UserRow.FromEntity(user));
    }

    /// <summary>
    /// ユーザーを削除
    /// </summary>
    public async Task DeleteAsync(UserId userId, CancellationToken cancellationToken = default)
    {
        const string sql = "DELETE FROM Users WHERE UserId = @UserId";

        using var connection = _connectionFactory.CreateConnection();
        await connection.ExecuteAsync(sql, new { UserId = userId.Value });
    }

    /// <summary>
    /// Dapper用のUserマッピングクラス
    /// </summary>
    private class UserRow
    {
        public string UserId { get; set; } = string.Empty;
        public string Name { get; set; } = string.Empty;
        public string HashedPassword { get; set; } = string.Empty;
        public string Role { get; set; } = string.Empty;
        public bool IsActive { get; set; }
        public DateTime CreatedAt { get; set; }
        public DateTime UpdatedAt { get; set; }

        /// <summary>
        /// UserRowからUserエンティティに変換
        /// </summary>
        public User ToEntity()
        {
            var userId = new UserId(UserId);
            var name = new Domain.ValueObjects.Name(Name);
            var password = Domain.ValueObjects.Password.FromHash(HashedPassword);
            var role = Enum.Parse<UserRole>(Role);

            var user = new User(userId, name, password, role);
            
            // IsActiveとタイムスタンプは内部状態として設定
            if (!IsActive)
            {
                user.Deactivate();
            }

            return user;
        }

        /// <summary>
        /// UserエンティティからUserRowに変換
        /// </summary>
        public static UserRow FromEntity(User user)
        {
            return new UserRow
            {
                UserId = user.UserId.Value,
                Name = user.Name.Value,
                HashedPassword = user.Password.HashedValue,
                Role = user.Role.ToString(),
                IsActive = user.IsActive,
                CreatedAt = user.CreatedAt,
                UpdatedAt = user.UpdatedAt
            };
        }
    }
}
