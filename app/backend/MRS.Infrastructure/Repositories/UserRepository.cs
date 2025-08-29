using System.Data;
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
