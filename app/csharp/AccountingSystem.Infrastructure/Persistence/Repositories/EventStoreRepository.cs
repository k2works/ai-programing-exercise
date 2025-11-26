namespace AccountingSystem.Infrastructure.Persistence.Repositories;

using AccountingSystem.Application.Exceptions;
using AccountingSystem.Application.Ports.Out;
using AccountingSystem.Domain.Events;
using Dapper;
using Npgsql;
using System.Text.Json;

/// <summary>
/// イベントストアリポジトリ実装
/// </summary>
public class EventStoreRepository : IEventStoreRepository
{
    private readonly string _connectionString;
    private readonly JsonSerializerOptions _jsonOptions = new()
    {
        PropertyNameCaseInsensitive = true
    };

    public EventStoreRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task SaveAsync(
        string aggregateId,
        IReadOnlyList<IEventSourcedDomainEvent> events,
        int expectedVersion)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.OpenAsync();

        var currentVersion = await GetCurrentVersionAsync(aggregateId);

        if (currentVersion != expectedVersion)
        {
            throw new ConcurrentModificationException(
                $"Expected version {expectedVersion} but was {currentVersion}"
            );
        }

        var sequenceNumber = currentVersion + 1;
        foreach (var @event in events)
        {
            var eventData = JsonSerializer.Serialize<object>(@event, _jsonOptions);

            var sql = @"
                INSERT INTO ""イベントストア"" (
                    ""集約ID"", ""集約種別"", ""イベント種別"", ""イベントバージョン"",
                    ""イベントデータ"", ""発生日時"", ""ユーザーID"", ""相関ID"", ""因果ID"", ""シーケンス番号""
                ) VALUES (
                    @AggregateId, @AggregateType, @EventType, @EventVersion,
                    @EventData::jsonb, @OccurredAt, @UserId, @CorrelationId, @CausationId, @SequenceNumber
                )
                ";

            try
            {
                await connection.ExecuteAsync(sql, new
                {
                    AggregateId = aggregateId,
                    AggregateType = "JournalEntry",
                    EventType = @event.EventType,
                    EventVersion = @event.EventVersion,
                    EventData = eventData,
                    OccurredAt = @event.OccurredAt,
                    UserId = @event.UserId,
                    CorrelationId = (string?)null,
                    CausationId = (string?)null,
                    SequenceNumber = sequenceNumber
                });
                sequenceNumber++;
            }
            catch (PostgresException ex) when (ex.SqlState == "23505") // unique_violation
            {
                throw new ConcurrentModificationException(
                    $"Concurrent modification detected for aggregate: {aggregateId}", ex
                );
            }
        }
    }

    public async Task<IReadOnlyList<IEventSourcedDomainEvent>> GetEventsAsync(string aggregateId)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.OpenAsync();

        var sql = @"
            SELECT ""イベントID"" as EventId,
                   ""集約ID"" as AggregateId,
                   ""集約種別"" as AggregateType,
                   ""イベント種別"" as EventType,
                   ""イベントバージョン"" as EventVersion,
                   ""イベントデータ""::text as EventData,
                   ""発生日時"" as OccurredAt,
                   ""ユーザーID"" as UserId,
                   ""相関ID"" as CorrelationId,
                   ""因果ID"" as CausationId,
                   ""シーケンス番号"" as SequenceNumber
            FROM ""イベントストア""
            WHERE ""集約ID"" = @AggregateId
            ORDER BY ""シーケンス番号""
            ";

        var entities = await connection.QueryAsync<EventStoreEntity>(sql, new { AggregateId = aggregateId });
        return entities.Select(ToDomainEvent).ToList();
    }

    public async Task<IReadOnlyList<IEventSourcedDomainEvent>> GetEventsUntilAsync(
        string aggregateId,
        DateTime pointInTime)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.OpenAsync();

        var sql = @"
            SELECT ""イベントID"" as EventId,
                   ""集約ID"" as AggregateId,
                   ""集約種別"" as AggregateType,
                   ""イベント種別"" as EventType,
                   ""イベントバージョン"" as EventVersion,
                   ""イベントデータ""::text as EventData,
                   ""発生日時"" as OccurredAt,
                   ""ユーザーID"" as UserId,
                   ""相関ID"" as CorrelationId,
                   ""因果ID"" as CausationId,
                   ""シーケンス番号"" as SequenceNumber
            FROM ""イベントストア""
            WHERE ""集約ID"" = @AggregateId
              AND ""発生日時"" <= @PointInTime
            ORDER BY ""シーケンス番号""
            ";

        var entities = await connection.QueryAsync<EventStoreEntity>(
            sql, new { AggregateId = aggregateId, PointInTime = pointInTime }
        );
        return entities.Select(ToDomainEvent).ToList();
    }

    public async Task<int> GetCurrentVersionAsync(string aggregateId)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.OpenAsync();

        var sql = @"
            SELECT MAX(""シーケンス番号"")
            FROM ""イベントストア""
            WHERE ""集約ID"" = @AggregateId
            ";

        var maxSequence = await connection.QuerySingleOrDefaultAsync<int?>(
            sql, new { AggregateId = aggregateId }
        );
        return maxSequence ?? 0;
    }

    /// <summary>
    /// イベントのデシリアライズ
    /// </summary>
    private IEventSourcedDomainEvent ToDomainEvent(EventStoreEntity entity)
    {
        return entity.EventType switch
        {
            "JournalEntryCreatedEvent" =>
                JsonSerializer.Deserialize<JournalEntryCreatedEvent>(entity.EventData, _jsonOptions)!,
            "JournalEntryApprovedEvent" =>
                JsonSerializer.Deserialize<JournalEntryApprovedEvent>(entity.EventData, _jsonOptions)!,
            "JournalEntryDeletedEvent" =>
                JsonSerializer.Deserialize<JournalEntryDeletedEvent>(entity.EventData, _jsonOptions)!,
            _ => throw new ArgumentException($"Unknown event type: {entity.EventType}")
        };
    }

    /// <summary>
    /// Dapper マッピング用の Entity クラス
    /// </summary>
    private class EventStoreEntity
    {
        public long EventId { get; set; }
        public string AggregateId { get; set; } = string.Empty;
        public string AggregateType { get; set; } = string.Empty;
        public string EventType { get; set; } = string.Empty;
        public int EventVersion { get; set; }
        public string EventData { get; set; } = string.Empty;
        public DateTime OccurredAt { get; set; }
        public string UserId { get; set; } = string.Empty;
        public string? CorrelationId { get; set; }
        public string? CausationId { get; set; }
        public int SequenceNumber { get; set; }
    }
}
