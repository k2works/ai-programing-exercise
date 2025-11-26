namespace AccountingSystem.Infrastructure.Persistence.Repositories;

using AccountingSystem.Application.Ports.Out;
using AccountingSystem.Domain.Aggregates;
using Dapper;
using Npgsql;
using System.Text.Json;

/// <summary>
/// スナップショットリポジトリ実装
/// </summary>
public class SnapshotRepository : ISnapshotRepository
{
    private readonly string _connectionString;
    private readonly JsonSerializerOptions _jsonOptions = new()
    {
        PropertyNameCaseInsensitive = true
    };

    public SnapshotRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task SaveSnapshotAsync(
        string aggregateId,
        int version,
        JournalEntryAggregate aggregate)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.OpenAsync();

        var snapshotData = JsonSerializer.Serialize(aggregate, _jsonOptions);

        var sql = @"
            INSERT INTO ""スナップショット"" (
                ""集約ID"", ""集約種別"", ""シーケンス番号"", ""スナップショットデータ""
            ) VALUES (
                @AggregateId, @AggregateType, @SequenceNumber, @SnapshotData::jsonb
            )
            ON CONFLICT (""集約ID"", ""シーケンス番号"") DO NOTHING
            ";

        await connection.ExecuteAsync(sql, new
        {
            AggregateId = aggregateId,
            AggregateType = "JournalEntry",
            SequenceNumber = version,
            SnapshotData = snapshotData
        });
    }

    public async Task<Snapshot?> GetLatestSnapshotAsync(string aggregateId)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.OpenAsync();

        var sql = @"
            SELECT ""スナップショットID"" as SnapshotId,
                   ""集約ID"" as AggregateId,
                   ""集約種別"" as AggregateType,
                   ""シーケンス番号"" as SequenceNumber,
                   ""スナップショットデータ""::text as SnapshotData,
                   ""作成日時"" as CreatedAt
            FROM ""スナップショット""
            WHERE ""集約ID"" = @AggregateId
            ORDER BY ""シーケンス番号"" DESC
            LIMIT 1
            ";

        var entity = await connection.QuerySingleOrDefaultAsync<SnapshotEntity>(
            sql, new { AggregateId = aggregateId }
        );

        if (entity == null)
        {
            return null;
        }

        var aggregate = JsonSerializer.Deserialize<JournalEntryAggregate>(
            entity.SnapshotData, _jsonOptions
        );

        return new Snapshot(aggregateId, entity.SequenceNumber, aggregate!);
    }

    /// <summary>
    /// Dapper マッピング用エンティティ
    /// </summary>
    private sealed class SnapshotEntity
    {
        public long SnapshotId { get; set; }
        public string AggregateId { get; set; } = string.Empty;
        public string AggregateType { get; set; } = string.Empty;
        public int SequenceNumber { get; set; }
        public string SnapshotData { get; set; } = string.Empty;
        public DateTime CreatedAt { get; set; }
    }
}
