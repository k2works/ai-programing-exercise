using AccountingSystem.Domain.Events;
using FluentAssertions;
using Xunit;

namespace AccountingSystem.Tests.Domain.Events;

/// <summary>
/// ドメインイベント基底クラスのテスト
/// </summary>
public class DomainEventTest
{
    [Fact]
    public void DomainEvent_ShouldGenerateUniqueEventId()
    {
        // Arrange & Act
        var event1 = new AccountCreatedEvent
        {
            AccountCode = "1100",
            AccountData = new Dictionary<string, object> { ["name"] = "現金" },
            UserId = "user1",
            UserName = "テストユーザー"
        };

        var event2 = new AccountCreatedEvent
        {
            AccountCode = "1200",
            AccountData = new Dictionary<string, object> { ["name"] = "普通預金" },
            UserId = "user1",
            UserName = "テストユーザー"
        };

        // Assert
        event1.EventId.Should().NotBeNullOrEmpty();
        event2.EventId.Should().NotBeNullOrEmpty();
        event1.EventId.Should().NotBe(event2.EventId);
    }

    [Fact]
    public void DomainEvent_ShouldSetOccurredAtToCurrentTime()
    {
        // Arrange
        var beforeCreation = DateTime.UtcNow;

        // Act
        var domainEvent = new AccountCreatedEvent
        {
            AccountCode = "1100",
            AccountData = new Dictionary<string, object>(),
            UserId = "user1",
            UserName = "テストユーザー"
        };

        var afterCreation = DateTime.UtcNow;

        // Assert
        domainEvent.OccurredAt.Should().BeOnOrAfter(beforeCreation);
        domainEvent.OccurredAt.Should().BeOnOrBefore(afterCreation);
    }

    [Fact]
    public void AccountCreatedEvent_ShouldReturnCorrectEventType()
    {
        // Arrange
        var domainEvent = new AccountCreatedEvent
        {
            AccountCode = "1100",
            AccountData = new Dictionary<string, object>(),
            UserId = "user1",
            UserName = "テストユーザー"
        };

        // Act
        var eventType = domainEvent.GetEventType();

        // Assert
        eventType.Should().Be("AccountCreated");
    }

    [Fact]
    public void AccountUpdatedEvent_ShouldReturnCorrectEventType()
    {
        // Arrange
        var domainEvent = new AccountUpdatedEvent
        {
            AccountCode = "1100",
            OldValues = new Dictionary<string, object>(),
            NewValues = new Dictionary<string, object>(),
            UserId = "user1",
            UserName = "テストユーザー"
        };

        // Act
        var eventType = domainEvent.GetEventType();

        // Assert
        eventType.Should().Be("AccountUpdated");
    }

    [Fact]
    public void AccountDeletedEvent_ShouldReturnCorrectEventType()
    {
        // Arrange
        var domainEvent = new AccountDeletedEvent
        {
            AccountCode = "1100",
            DeletedData = new Dictionary<string, object>(),
            UserId = "user1",
            UserName = "テストユーザー"
        };

        // Act
        var eventType = domainEvent.GetEventType();

        // Assert
        eventType.Should().Be("AccountDeleted");
    }

    [Fact]
    public void JournalCreatedEvent_ShouldReturnCorrectEventType()
    {
        // Arrange
        var domainEvent = new JournalCreatedEvent
        {
            JournalNo = "J2024-0001",
            JournalDate = new DateOnly(2024, 1, 1),
            JournalData = new Dictionary<string, object>(),
            UserId = "user1",
            UserName = "テストユーザー"
        };

        // Act
        var eventType = domainEvent.GetEventType();

        // Assert
        eventType.Should().Be("JournalCreated");
    }

    [Fact]
    public void JournalDeletedEvent_ShouldReturnCorrectEventType()
    {
        // Arrange
        var domainEvent = new JournalDeletedEvent
        {
            JournalNo = "J2024-0001",
            DeletedData = new Dictionary<string, object>(),
            UserId = "user1",
            UserName = "テストユーザー"
        };

        // Act
        var eventType = domainEvent.GetEventType();

        // Assert
        eventType.Should().Be("JournalDeleted");
    }

    [Fact]
    public void AccountCreatedEvent_ShouldStoreAllProperties()
    {
        // Arrange
        var accountData = new Dictionary<string, object>
        {
            ["name"] = "現金",
            ["category"] = "資産"
        };

        // Act
        var domainEvent = new AccountCreatedEvent
        {
            AccountCode = "1100",
            AccountData = accountData,
            UserId = "user123",
            UserName = "山田太郎",
            IpAddress = "192.168.1.1"
        };

        // Assert
        domainEvent.AccountCode.Should().Be("1100");
        domainEvent.AccountData.Should().BeEquivalentTo(accountData);
        domainEvent.UserId.Should().Be("user123");
        domainEvent.UserName.Should().Be("山田太郎");
        domainEvent.IpAddress.Should().Be("192.168.1.1");
    }

    [Fact]
    public void AccountUpdatedEvent_ShouldStoreOldAndNewValues()
    {
        // Arrange
        var oldValues = new Dictionary<string, object> { ["name"] = "旧名称" };
        var newValues = new Dictionary<string, object> { ["name"] = "新名称" };

        // Act
        var domainEvent = new AccountUpdatedEvent
        {
            AccountCode = "1100",
            OldValues = oldValues,
            NewValues = newValues,
            UserId = "user123",
            UserName = "山田太郎"
        };

        // Assert
        domainEvent.OldValues.Should().BeEquivalentTo(oldValues);
        domainEvent.NewValues.Should().BeEquivalentTo(newValues);
    }

    [Fact]
    public void AccountDeletedEvent_ShouldStoreReasonIfProvided()
    {
        // Arrange & Act
        var domainEvent = new AccountDeletedEvent
        {
            AccountCode = "1100",
            DeletedData = new Dictionary<string, object>(),
            UserId = "user123",
            UserName = "山田太郎",
            Reason = "重複登録のため削除"
        };

        // Assert
        domainEvent.Reason.Should().Be("重複登録のため削除");
    }

    [Fact]
    public void JournalCreatedEvent_ShouldStoreJournalDate()
    {
        // Arrange
        var journalDate = new DateOnly(2024, 3, 15);

        // Act
        var domainEvent = new JournalCreatedEvent
        {
            JournalNo = "J2024-0001",
            JournalDate = journalDate,
            JournalData = new Dictionary<string, object>(),
            UserId = "user123",
            UserName = "山田太郎"
        };

        // Assert
        domainEvent.JournalDate.Should().Be(journalDate);
    }
}
