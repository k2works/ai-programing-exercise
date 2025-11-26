using AccountingSystem.Domain.Aggregates;
using AccountingSystem.Domain.Events;
using FluentAssertions;
using Xunit;

namespace AccountingSystem.Tests.Domain;

/// <summary>
/// JournalEntryAggregate のテスト
/// </summary>
public class JournalEntryAggregateTest
{
    [Fact(DisplayName = "仕訳を作成できる")]
    public void CreateJournalEntry()
    {
        // Arrange
        var id = Guid.NewGuid().ToString();
        var lineItems = new List<JournalEntryAggregate.LineItem>
        {
            new("1000", JournalEntryAggregate.DebitCredit.DEBIT, 1000m),
            new("2000", JournalEntryAggregate.DebitCredit.CREDIT, 1000m)
        };

        // Act
        var aggregate = JournalEntryAggregate.Create(
            id,
            new DateOnly(2025, 1, 15),
            "テスト仕訳",
            lineItems,
            "user1"
        );

        // Assert
        aggregate.Id.Should().Be(id);
        aggregate.EntryDate.Should().Be(new DateOnly(2025, 1, 15));
        aggregate.Description.Should().Be("テスト仕訳");
        aggregate.Status.Should().Be(JournalEntryAggregate.JournalEntryStatus.DRAFT);
        aggregate.Deleted.Should().BeFalse();
        aggregate.LineItems.Should().HaveCount(2);
        aggregate.UncommittedEvents.Should().HaveCount(1);
        aggregate.UncommittedEvents[0].Should().BeOfType<JournalEntryCreatedEvent>();
    }

    [Fact(DisplayName = "借方貸方が一致しない場合はエラー")]
    public void CreateJournalEntryUnbalanced()
    {
        // Arrange
        var id = Guid.NewGuid().ToString();
        var lineItems = new List<JournalEntryAggregate.LineItem>
        {
            new("1000", JournalEntryAggregate.DebitCredit.DEBIT, 1000m),
            new("2000", JournalEntryAggregate.DebitCredit.CREDIT, 500m)
        };

        // Act & Assert
        var act = () => JournalEntryAggregate.Create(
            id,
            new DateOnly(2025, 1, 15),
            "テスト仕訳",
            lineItems,
            "user1"
        );

        act.Should().Throw<ArgumentException>()
            .WithMessage("*借方合計と貸方合計が一致しません*");
    }

    [Fact(DisplayName = "仕訳を承認できる")]
    public void ApproveJournalEntry()
    {
        // Arrange
        var id = Guid.NewGuid().ToString();
        var lineItems = new List<JournalEntryAggregate.LineItem>
        {
            new("1000", JournalEntryAggregate.DebitCredit.DEBIT, 1000m),
            new("2000", JournalEntryAggregate.DebitCredit.CREDIT, 1000m)
        };
        var aggregate = JournalEntryAggregate.Create(
            id,
            new DateOnly(2025, 1, 15),
            "テスト仕訳",
            lineItems,
            "user1"
        );
        aggregate.MarkEventsAsCommitted();

        // Act
        aggregate.Approve("approver1", "承認します");

        // Assert
        aggregate.Status.Should().Be(JournalEntryAggregate.JournalEntryStatus.APPROVED);
        aggregate.UncommittedEvents.Should().HaveCount(1);
        aggregate.UncommittedEvents[0].Should().BeOfType<JournalEntryApprovedEvent>();
    }

    [Fact(DisplayName = "既に承認済みの仕訳は承認できない")]
    public void CannotApproveAlreadyApproved()
    {
        // Arrange
        var id = Guid.NewGuid().ToString();
        var lineItems = new List<JournalEntryAggregate.LineItem>
        {
            new("1000", JournalEntryAggregate.DebitCredit.DEBIT, 1000m),
            new("2000", JournalEntryAggregate.DebitCredit.CREDIT, 1000m)
        };
        var aggregate = JournalEntryAggregate.Create(
            id,
            new DateOnly(2025, 1, 15),
            "テスト仕訳",
            lineItems,
            "user1"
        );
        aggregate.Approve("approver1", "承認します");

        // Act & Assert
        var act = () => aggregate.Approve("approver2", "再承認");

        act.Should().Throw<InvalidOperationException>()
            .WithMessage("すでに承認済みです");
    }

    [Fact(DisplayName = "仕訳を削除できる")]
    public void DeleteJournalEntry()
    {
        // Arrange
        var id = Guid.NewGuid().ToString();
        var lineItems = new List<JournalEntryAggregate.LineItem>
        {
            new("1000", JournalEntryAggregate.DebitCredit.DEBIT, 1000m),
            new("2000", JournalEntryAggregate.DebitCredit.CREDIT, 1000m)
        };
        var aggregate = JournalEntryAggregate.Create(
            id,
            new DateOnly(2025, 1, 15),
            "テスト仕訳",
            lineItems,
            "user1"
        );
        aggregate.MarkEventsAsCommitted();

        // Act
        aggregate.Delete("誤入力のため", "user1");

        // Assert
        aggregate.Deleted.Should().BeTrue();
        aggregate.UncommittedEvents.Should().HaveCount(1);
        aggregate.UncommittedEvents[0].Should().BeOfType<JournalEntryDeletedEvent>();
    }

    [Fact(DisplayName = "イベントから Aggregate を再生できる")]
    public void ReplayEvents()
    {
        // Arrange
        var id = Guid.NewGuid().ToString();
        var events = new List<IEventSourcedDomainEvent>
        {
            new JournalEntryCreatedEvent
            {
                JournalEntryId = id,
                EntryDate = new DateOnly(2025, 1, 15),
                Description = "テスト仕訳",
                LineItems = new List<JournalEntryCreatedEvent.JournalEntryLineItem>
                {
                    new() { AccountCode = "1000", DebitCredit = JournalEntryCreatedEvent.DebitCreditType.DEBIT, Amount = 1000m },
                    new() { AccountCode = "2000", DebitCredit = JournalEntryCreatedEvent.DebitCreditType.CREDIT, Amount = 1000m }
                },
                UserId = "user1",
                OccurredAt = DateTime.UtcNow
            },
            new JournalEntryApprovedEvent
            {
                JournalEntryId = id,
                ApprovedBy = "approver1",
                ApprovalComment = "承認します",
                OccurredAt = DateTime.UtcNow,
                UserId = "approver1"
            }
        };

        // Act
        var aggregate = JournalEntryAggregate.Replay(events);

        // Assert
        aggregate.Id.Should().Be(id);
        aggregate.Status.Should().Be(JournalEntryAggregate.JournalEntryStatus.APPROVED);
        aggregate.Version.Should().Be(2);
        aggregate.UncommittedEvents.Should().BeEmpty();
    }
}
