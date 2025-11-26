using AccountingSystem.Infrastructure.EventHandlers;
using AccountingSystem.Application.Services;
using AccountingSystem.Infrastructure.Persistence.Repositories;
using AccountingSystem.Domain.Models.Audit;
using AccountingSystem.Domain.Events;
using FluentAssertions;
using Microsoft.Extensions.Logging;
using Moq;
using Xunit;

namespace AccountingSystem.Tests.Infrastructure.EventHandlers;

/// <summary>
/// AuditEventHandler の統合テスト
/// DatabaseTestBase を使用して実際のデータベースに書き込みを検証
/// </summary>
public class AuditEventHandlerTest : DatabaseTestBase
{
    private AuditLogRepository CreateRepository() => new AuditLogRepository(ConnectionString);

    private AuditEventHandler CreateHandler()
    {
        var repository = CreateRepository();
        var service = new AuditLogService(repository);
        var mockLogger = new Mock<ILogger<AuditEventHandler>>();
        return new AuditEventHandler(service, mockLogger.Object);
    }

    [Fact]
    public async Task Handle_AccountCreatedEvent_ShouldRecordAuditLog()
    {
        // Arrange
        var handler = CreateHandler();
        var accountData = new Dictionary<string, object>
        {
            ["code"] = "1100",
            ["name"] = "現金"
        };

        var notification = new AccountCreatedEvent
        {
            AccountCode = "1100",
            AccountData = accountData,
            UserId = "user123",
            UserName = "山田太郎",
            IpAddress = "192.168.1.1"
        };

        // Act
        await handler.Handle(notification, CancellationToken.None);

        // Assert - データベースから取得して検証
        var repository = CreateRepository();
        var logs = await repository.FindByEntityAsync("Account", "1100");
        logs.Should().HaveCount(1);

        var auditLog = logs[0];
        auditLog.EntityType.Should().Be("Account");
        auditLog.EntityId.Should().Be("1100");
        auditLog.Action.Should().Be(AuditAction.CREATE);
        auditLog.UserId.Should().Be("user123");
        auditLog.UserName.Should().Be("山田太郎");
        auditLog.IpAddress.Should().Be("192.168.1.1");
    }

    [Fact]
    public async Task Handle_AccountUpdatedEvent_ShouldRecordAuditLogWithChanges()
    {
        // Arrange
        var handler = CreateHandler();
        var oldValues = new Dictionary<string, object>
        {
            ["name"] = "現金"
        };
        var newValues = new Dictionary<string, object>
        {
            ["name"] = "小口現金"
        };

        var notification = new AccountUpdatedEvent
        {
            AccountCode = "1100",
            OldValues = oldValues,
            NewValues = newValues,
            UserId = "user123",
            UserName = "山田太郎",
            IpAddress = "192.168.1.1"
        };

        // Act
        await handler.Handle(notification, CancellationToken.None);

        // Assert - データベースから取得して検証
        var repository = CreateRepository();
        var logs = await repository.FindByEntityAsync("Account", "1100");
        logs.Should().HaveCount(1);

        var auditLog = logs[0];
        auditLog.EntityType.Should().Be("Account");
        auditLog.EntityId.Should().Be("1100");
        auditLog.Action.Should().Be(AuditAction.UPDATE);
    }

    [Fact]
    public async Task Handle_AccountDeletedEvent_ShouldRecordAuditLogWithReason()
    {
        // Arrange
        var handler = CreateHandler();
        var deletedData = new Dictionary<string, object>
        {
            ["code"] = "1100",
            ["name"] = "現金"
        };

        var notification = new AccountDeletedEvent
        {
            AccountCode = "1100",
            DeletedData = deletedData,
            UserId = "user123",
            UserName = "山田太郎",
            Reason = "重複登録のため削除",
            IpAddress = "192.168.1.1"
        };

        // Act
        await handler.Handle(notification, CancellationToken.None);

        // Assert - データベースから取得して検証
        var repository = CreateRepository();
        var logs = await repository.FindByEntityAsync("Account", "1100");
        logs.Should().HaveCount(1);

        var auditLog = logs[0];
        auditLog.EntityType.Should().Be("Account");
        auditLog.EntityId.Should().Be("1100");
        auditLog.Action.Should().Be(AuditAction.DELETE);
        auditLog.Reason.Should().Be("重複登録のため削除");
    }

    [Fact]
    public async Task Handle_JournalCreatedEvent_ShouldRecordAuditLog()
    {
        // Arrange
        var handler = CreateHandler();
        var journalData = new Dictionary<string, object>
        {
            ["journalNo"] = "J2024-0001",
            ["date"] = "2024-01-01",
            ["amount"] = 100000
        };

        var notification = new JournalCreatedEvent
        {
            JournalNo = "J2024-0001",
            JournalDate = new DateOnly(2024, 1, 1),
            JournalData = journalData,
            UserId = "user123",
            UserName = "山田太郎",
            IpAddress = "192.168.1.1"
        };

        // Act
        await handler.Handle(notification, CancellationToken.None);

        // Assert - データベースから取得して検証
        var repository = CreateRepository();
        var logs = await repository.FindByEntityAsync("Journal", "J2024-0001");
        logs.Should().HaveCount(1);

        var auditLog = logs[0];
        auditLog.EntityType.Should().Be("Journal");
        auditLog.EntityId.Should().Be("J2024-0001");
        auditLog.Action.Should().Be(AuditAction.CREATE);
    }

    [Fact]
    public async Task Handle_JournalDeletedEvent_ShouldRecordAuditLog()
    {
        // Arrange
        var handler = CreateHandler();
        var deletedData = new Dictionary<string, object>
        {
            ["journalNo"] = "J2024-0001",
            ["date"] = "2024-01-01"
        };

        var notification = new JournalDeletedEvent
        {
            JournalNo = "J2024-0001",
            DeletedData = deletedData,
            UserId = "user123",
            UserName = "山田太郎",
            Reason = "入力ミスのため削除",
            IpAddress = "192.168.1.1"
        };

        // Act
        await handler.Handle(notification, CancellationToken.None);

        // Assert - データベースから取得して検証
        var repository = CreateRepository();
        var logs = await repository.FindByEntityAsync("Journal", "J2024-0001");
        logs.Should().HaveCount(1);

        var auditLog = logs[0];
        auditLog.EntityType.Should().Be("Journal");
        auditLog.EntityId.Should().Be("J2024-0001");
        auditLog.Action.Should().Be(AuditAction.DELETE);
        auditLog.Reason.Should().Be("入力ミスのため削除");
    }

    [Fact]
    public async Task Handle_WhenIpAddressIsNull_ShouldStillRecordAuditLog()
    {
        // Arrange
        var handler = CreateHandler();
        var notification = new AccountCreatedEvent
        {
            AccountCode = "1200",
            AccountData = new Dictionary<string, object>(),
            UserId = "user123",
            UserName = "山田太郎",
            IpAddress = null
        };

        // Act
        await handler.Handle(notification, CancellationToken.None);

        // Assert - データベースから取得して検証
        var repository = CreateRepository();
        var logs = await repository.FindByEntityAsync("Account", "1200");
        logs.Should().HaveCount(1);

        var auditLog = logs[0];
        auditLog.IpAddress.Should().BeNull();
    }

    [Fact]
    public async Task Handle_ShouldLogInformationMessage()
    {
        // Arrange
        var repository = CreateRepository();
        var service = new AuditLogService(repository);
        var mockLogger = new Mock<ILogger<AuditEventHandler>>();
        var handler = new AuditEventHandler(service, mockLogger.Object);

        var notification = new AccountCreatedEvent
        {
            AccountCode = "1300",
            AccountData = new Dictionary<string, object>(),
            UserId = "user123",
            UserName = "山田太郎"
        };

        // Act
        await handler.Handle(notification, CancellationToken.None);

        // Assert
        mockLogger.Verify(
            x => x.Log(
                LogLevel.Information,
                It.IsAny<EventId>(),
                It.Is<It.IsAnyType>((v, t) => v.ToString()!.Contains("AccountCreatedEvent")),
                It.IsAny<Exception>(),
                It.IsAny<Func<It.IsAnyType, Exception?, string>>()),
            Times.Once);
    }
}
