using AccountingSystem.Domain.Aggregates;
using AccountingSystem.Domain.Events;
using AccountingSystem.Infrastructure.Persistence.Repositories;
using FluentAssertions;
using Xunit;

namespace AccountingSystem.Tests.Infrastructure.Repositories;

/// <summary>
/// イベントストアリポジトリのテスト
/// </summary>
public class EventStoreRepositoryTest : DatabaseTestBase
{
    [Fact(DisplayName = "イベントを保存して取得できる")]
    public async Task SaveAndGetEvents()
    {
        // Arrange
        var repository = new EventStoreRepository(ConnectionString);
        var aggregateId = Guid.NewGuid().ToString();

        var @event = new JournalEntryCreatedEvent
        {
            JournalEntryId = aggregateId,
            EntryDate = new DateOnly(2025, 1, 15),
            Description = "テスト仕訳",
            LineItems = new List<JournalEntryCreatedEvent.JournalEntryLineItem>
            {
                new() { AccountCode = "1000", DebitCredit = JournalEntryCreatedEvent.DebitCreditType.DEBIT, Amount = 1000m },
                new() { AccountCode = "2000", DebitCredit = JournalEntryCreatedEvent.DebitCreditType.CREDIT, Amount = 1000m }
            },
            UserId = "user1",
            OccurredAt = DateTime.UtcNow
        };

        // Act
        await repository.SaveAsync(aggregateId, new List<IEventSourcedDomainEvent> { @event }, 0);
        var events = await repository.GetEventsAsync(aggregateId);

        // Assert
        events.Should().HaveCount(1);
        var savedEvent = events[0].Should().BeOfType<JournalEntryCreatedEvent>().Subject;
        savedEvent.JournalEntryId.Should().Be(aggregateId);
        savedEvent.Description.Should().Be("テスト仕訳");
        savedEvent.LineItems.Should().HaveCount(2);
    }

    [Fact(DisplayName = "複数のイベントを順序を保持して取得できる")]
    public async Task GetEventsInOrder()
    {
        // Arrange
        var repository = new EventStoreRepository(ConnectionString);
        var aggregateId = Guid.NewGuid().ToString();

        var createEvent = new JournalEntryCreatedEvent
        {
            JournalEntryId = aggregateId,
            EntryDate = new DateOnly(2025, 1, 15),
            Description = "テスト仕訳",
            LineItems = new List<JournalEntryCreatedEvent.JournalEntryLineItem>
            {
                new() { AccountCode = "1000", DebitCredit = JournalEntryCreatedEvent.DebitCreditType.DEBIT, Amount = 1000m },
                new() { AccountCode = "2000", DebitCredit = JournalEntryCreatedEvent.DebitCreditType.CREDIT, Amount = 1000m }
            },
            UserId = "user1",
            OccurredAt = DateTime.UtcNow
        };

        var approveEvent = new JournalEntryApprovedEvent
        {
            JournalEntryId = aggregateId,
            ApprovedBy = "approver1",
            ApprovalComment = "承認します",
            OccurredAt = DateTime.UtcNow,
            UserId = "approver1"
        };

        // Act
        await repository.SaveAsync(aggregateId, new List<IEventSourcedDomainEvent> { createEvent }, 0);
        await repository.SaveAsync(aggregateId, new List<IEventSourcedDomainEvent> { approveEvent }, 1);
        var events = await repository.GetEventsAsync(aggregateId);

        // Assert
        events.Should().HaveCount(2);
        events[0].Should().BeOfType<JournalEntryCreatedEvent>();
        events[1].Should().BeOfType<JournalEntryApprovedEvent>();
    }

    [Fact(DisplayName = "現在のバージョンを取得できる")]
    public async Task GetCurrentVersion()
    {
        // Arrange
        var repository = new EventStoreRepository(ConnectionString);
        var aggregateId = Guid.NewGuid().ToString();

        var @event = new JournalEntryCreatedEvent
        {
            JournalEntryId = aggregateId,
            EntryDate = new DateOnly(2025, 1, 15),
            Description = "テスト仕訳",
            LineItems = new List<JournalEntryCreatedEvent.JournalEntryLineItem>
            {
                new() { AccountCode = "1000", DebitCredit = JournalEntryCreatedEvent.DebitCreditType.DEBIT, Amount = 1000m },
                new() { AccountCode = "2000", DebitCredit = JournalEntryCreatedEvent.DebitCreditType.CREDIT, Amount = 1000m }
            },
            UserId = "user1",
            OccurredAt = DateTime.UtcNow
        };

        // Act
        var versionBefore = await repository.GetCurrentVersionAsync(aggregateId);
        await repository.SaveAsync(aggregateId, new List<IEventSourcedDomainEvent> { @event }, 0);
        var versionAfter = await repository.GetCurrentVersionAsync(aggregateId);

        // Assert
        versionBefore.Should().Be(0);
        versionAfter.Should().Be(1);
    }
}
