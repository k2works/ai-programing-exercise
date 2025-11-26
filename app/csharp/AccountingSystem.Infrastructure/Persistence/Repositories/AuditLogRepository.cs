using AccountingSystem.Domain.Entities;
using AccountingSystem.Application.Ports.Out;
using AccountingSystem.Domain.Models.Audit;
using Dapper;
using Npgsql;
using System.Text.Json;

namespace AccountingSystem.Infrastructure.Persistence.Repositories;

/// <summary>
/// 監査ログリポジトリ実装
/// </summary>
public class AuditLogRepository : IAuditLogRepository
{
    private readonly string _connectionString;

    public AuditLogRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task<AuditLog> InsertAsync(AuditLog auditLog)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.OpenAsync();

        var sql = @"
            INSERT INTO ""監査ログ"" (
                ""エンティティ種別"", ""エンティティID"", ""アクション"", ""ユーザーID"", ""ユーザー名"",
                ""タイムスタンプ"", ""変更前値"", ""変更後値"", ""変更内容"", ""理由"",
                ""IPアドレス"", ""ユーザーエージェント""
            ) VALUES (
                @EntityType, @EntityId, @Action, @UserId, @UserName,
                @Timestamp, @OldValues::jsonb, @NewValues::jsonb,
                @Changes::jsonb, @Reason, @IpAddress, @UserAgent
            )
            RETURNING ""監査ログID""";

        var id = await connection.ExecuteScalarAsync<long>(sql, new
        {
            auditLog.EntityType,
            auditLog.EntityId,
            Action = auditLog.Action.ToString(),
            auditLog.UserId,
            auditLog.UserName,
            auditLog.Timestamp,
            OldValues = SerializeToJson(auditLog.OldValues),
            NewValues = SerializeToJson(auditLog.NewValues),
            Changes = SerializeToJson(auditLog.Changes),
            auditLog.Reason,
            auditLog.IpAddress,
            auditLog.UserAgent
        });

        return auditLog with { Id = id };
    }

    public async Task<IReadOnlyList<AuditLog>> FindByEntityAsync(
        string entityType,
        string entityId)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.OpenAsync();

        var sql = @"
            SELECT
                ""監査ログID"" AS Id,
                ""エンティティ種別"" AS EntityType,
                ""エンティティID"" AS EntityId,
                ""アクション"" AS Action,
                ""ユーザーID"" AS UserId,
                ""ユーザー名"" AS UserName,
                ""タイムスタンプ"" AS Timestamp,
                ""変更前値"" AS OldValues,
                ""変更後値"" AS NewValues,
                ""変更内容"" AS Changes,
                ""理由"" AS Reason,
                ""IPアドレス"" AS IpAddress,
                ""ユーザーエージェント"" AS UserAgent
            FROM ""監査ログ""
            WHERE ""エンティティ種別"" = @EntityType
              AND ""エンティティID"" = @EntityId
            ORDER BY ""タイムスタンプ"" DESC";

        var entities = await connection.QueryAsync<AuditLogEntity>(sql, new
        {
            EntityType = entityType,
            EntityId = entityId
        });

        return entities.Select(ToDomain).ToList();
    }

    public async Task<IReadOnlyList<AuditLog>> FindByUserAsync(
        string userId,
        DateTime startDate,
        DateTime endDate)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.OpenAsync();

        var sql = @"
            SELECT
                ""監査ログID"" AS Id,
                ""エンティティ種別"" AS EntityType,
                ""エンティティID"" AS EntityId,
                ""アクション"" AS Action,
                ""ユーザーID"" AS UserId,
                ""ユーザー名"" AS UserName,
                ""タイムスタンプ"" AS Timestamp,
                ""変更前値"" AS OldValues,
                ""変更後値"" AS NewValues,
                ""変更内容"" AS Changes,
                ""理由"" AS Reason,
                ""IPアドレス"" AS IpAddress,
                ""ユーザーエージェント"" AS UserAgent
            FROM ""監査ログ""
            WHERE ""ユーザーID"" = @UserId
              AND ""タイムスタンプ"" BETWEEN @StartDate AND @EndDate
            ORDER BY ""タイムスタンプ"" DESC";

        var entities = await connection.QueryAsync<AuditLogEntity>(sql, new
        {
            UserId = userId,
            StartDate = startDate,
            EndDate = endDate
        });

        return entities.Select(ToDomain).ToList();
    }

    public async Task<IReadOnlyList<AuditLog>> FindByPeriodAsync(
        DateTime startDate,
        DateTime endDate,
        int limit)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.OpenAsync();

        var sql = @"
            SELECT
                ""監査ログID"" AS Id,
                ""エンティティ種別"" AS EntityType,
                ""エンティティID"" AS EntityId,
                ""アクション"" AS Action,
                ""ユーザーID"" AS UserId,
                ""ユーザー名"" AS UserName,
                ""タイムスタンプ"" AS Timestamp,
                ""変更前値"" AS OldValues,
                ""変更後値"" AS NewValues,
                ""変更内容"" AS Changes,
                ""理由"" AS Reason,
                ""IPアドレス"" AS IpAddress,
                ""ユーザーエージェント"" AS UserAgent
            FROM ""監査ログ""
            WHERE ""タイムスタンプ"" BETWEEN @StartDate AND @EndDate
            ORDER BY ""タイムスタンプ"" DESC
            LIMIT @Limit";

        var entities = await connection.QueryAsync<AuditLogEntity>(sql, new
        {
            StartDate = startDate,
            EndDate = endDate,
            Limit = limit
        });

        return entities.Select(ToDomain).ToList();
    }

    public async Task<IReadOnlyList<AuditLog>> FindByActionAsync(
        AuditAction action,
        DateTime startDate,
        DateTime endDate,
        int limit)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.OpenAsync();

        var sql = @"
            SELECT
                ""監査ログID"" AS Id,
                ""エンティティ種別"" AS EntityType,
                ""エンティティID"" AS EntityId,
                ""アクション"" AS Action,
                ""ユーザーID"" AS UserId,
                ""ユーザー名"" AS UserName,
                ""タイムスタンプ"" AS Timestamp,
                ""変更前値"" AS OldValues,
                ""変更後値"" AS NewValues,
                ""変更内容"" AS Changes,
                ""理由"" AS Reason,
                ""IPアドレス"" AS IpAddress,
                ""ユーザーエージェント"" AS UserAgent
            FROM ""監査ログ""
            WHERE ""アクション"" = @Action
              AND ""タイムスタンプ"" BETWEEN @StartDate AND @EndDate
            ORDER BY ""タイムスタンプ"" DESC
            LIMIT @Limit";

        var entities = await connection.QueryAsync<AuditLogEntity>(sql, new
        {
            Action = action.ToString(),
            StartDate = startDate,
            EndDate = endDate,
            Limit = limit
        });

        return entities.Select(ToDomain).ToList();
    }

    /// <summary>
    /// Entity → Domain Model 変換
    /// </summary>
    private static AuditLog ToDomain(AuditLogEntity entity)
    {
        return new AuditLog
        {
            Id = entity.Id,
            EntityType = entity.EntityType,
            EntityId = entity.EntityId,
            Action = Enum.Parse<AuditAction>(entity.Action),
            UserId = entity.UserId,
            UserName = entity.UserName,
            Timestamp = entity.Timestamp,
            OldValues = DeserializeFromJson(entity.OldValues),
            NewValues = DeserializeFromJson(entity.NewValues),
            Changes = DeserializeFromJson(entity.Changes),
            Reason = entity.Reason,
            IpAddress = entity.IpAddress,
            UserAgent = entity.UserAgent
        };
    }

    /// <summary>
    /// Dictionary を JSON 文字列にシリアライズ
    /// </summary>
    private static string? SerializeToJson(Dictionary<string, object>? dict)
    {
        return dict != null ? JsonSerializer.Serialize(dict) : null;
    }

    /// <summary>
    /// JSON 文字列を Dictionary にデシリアライズ
    /// </summary>
    private static Dictionary<string, object>? DeserializeFromJson(string? json)
    {
        return json != null
            ? JsonSerializer.Deserialize<Dictionary<string, object>>(json)
            : null;
    }

    /// <summary>
    /// Dapper マッピング用の Entity クラス
    /// </summary>
    private class AuditLogEntity
    {
        public long Id { get; set; }
        public string EntityType { get; set; } = string.Empty;
        public string EntityId { get; set; } = string.Empty;
        public string Action { get; set; } = string.Empty;
        public string UserId { get; set; } = string.Empty;
        public string UserName { get; set; } = string.Empty;
        public DateTime Timestamp { get; set; }
        public string? OldValues { get; set; }      // JSONB → String
        public string? NewValues { get; set; }      // JSONB → String
        public string? Changes { get; set; }        // JSONB → String
        public string? Reason { get; set; }
        public string? IpAddress { get; set; }
        public string? UserAgent { get; set; }
    }
}
