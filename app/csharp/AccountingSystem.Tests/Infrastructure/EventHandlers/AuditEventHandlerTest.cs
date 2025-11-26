using AccountingSystem.Infrastructure.EventHandlers;
using AccountingSystem.Application.Ports.In;
using AccountingSystem.Domain.Models.Audit;
using AccountingSystem.Domain.Events;
using AccountingSystem.Infrastructure.Persistence.Dapper.Entities;
using FluentAssertions;
using Microsoft.Extensions.Logging;
using Moq;
using Xunit;

namespace AccountingSystem.Tests.Infrastructure.EventHandlers;

/// <summary>
/// AuditEventHandler のユニットテスト
/// </summary>
public class AuditEventHandlerTest
{
    private readonly Mock<IAuditLogService> _mockAuditLogService;
    private readonly Mock<ILogger<AuditEventHandler>> _mockLogger;
    private readonly AuditEventHandler _handler;

    public AuditEventHandlerTest()
    {
        _mockAuditLogService = new Mock<IAuditLogService>();
        _mockLogger = new Mock<ILogger<AuditEventHandler>>();
        _handler = new AuditEventHandler(_mockAuditLogService.Object, _mockLogger.Object);
    }

    [Fact]
    public async Task Handle_AccountCreatedEvent_ShouldRecordAuditLog()
    {
        // Arrange
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

        AuditLog? capturedAuditLog = null;
        _mockAuditLogService
            .Setup(x => x.RecordAsync(It.IsAny<AuditLog>()))
            .Callback<AuditLog>(log => capturedAuditLog = log)
            .Returns(Task.CompletedTask);

        // Act
        await _handler.Handle(notification, CancellationToken.None);

        // Assert
        _mockAuditLogService.Verify(x => x.RecordAsync(It.IsAny<AuditLog>()), Times.Once);
        capturedAuditLog.Should().NotBeNull();
        capturedAuditLog!.EntityType.Should().Be("Account");
        capturedAuditLog.EntityId.Should().Be("1100");
        capturedAuditLog.Action.Should().Be(AuditAction.CREATE);
        capturedAuditLog.UserId.Should().Be("user123");
        capturedAuditLog.UserName.Should().Be("山田太郎");
        capturedAuditLog.IpAddress.Should().Be("192.168.1.1");
        // AuditLog.Create は Changes プロパティにデータを格納
        capturedAuditLog.Changes.Should().BeEquivalentTo(accountData);
    }

    [Fact]
    public async Task Handle_AccountUpdatedEvent_ShouldRecordAuditLogWithChanges()
    {
        // Arrange
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

        AuditLog? capturedAuditLog = null;
        _mockAuditLogService
            .Setup(x => x.RecordAsync(It.IsAny<AuditLog>()))
            .Callback<AuditLog>(log => capturedAuditLog = log)
            .Returns(Task.CompletedTask);

        // Act
        await _handler.Handle(notification, CancellationToken.None);

        // Assert
        _mockAuditLogService.Verify(x => x.RecordAsync(It.IsAny<AuditLog>()), Times.Once);
        capturedAuditLog.Should().NotBeNull();
        capturedAuditLog!.EntityType.Should().Be("Account");
        capturedAuditLog.EntityId.Should().Be("1100");
        capturedAuditLog.Action.Should().Be(AuditAction.UPDATE);
        capturedAuditLog.OldValues.Should().BeEquivalentTo(oldValues);
        capturedAuditLog.NewValues.Should().BeEquivalentTo(newValues);
    }

    [Fact]
    public async Task Handle_AccountDeletedEvent_ShouldRecordAuditLogWithReason()
    {
        // Arrange
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

        AuditLog? capturedAuditLog = null;
        _mockAuditLogService
            .Setup(x => x.RecordAsync(It.IsAny<AuditLog>()))
            .Callback<AuditLog>(log => capturedAuditLog = log)
            .Returns(Task.CompletedTask);

        // Act
        await _handler.Handle(notification, CancellationToken.None);

        // Assert
        _mockAuditLogService.Verify(x => x.RecordAsync(It.IsAny<AuditLog>()), Times.Once);
        capturedAuditLog.Should().NotBeNull();
        capturedAuditLog!.EntityType.Should().Be("Account");
        capturedAuditLog.EntityId.Should().Be("1100");
        capturedAuditLog.Action.Should().Be(AuditAction.DELETE);
        capturedAuditLog.OldValues.Should().BeEquivalentTo(deletedData);
        capturedAuditLog.Reason.Should().Be("重複登録のため削除");
    }

    [Fact]
    public async Task Handle_JournalCreatedEvent_ShouldRecordAuditLog()
    {
        // Arrange
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

        AuditLog? capturedAuditLog = null;
        _mockAuditLogService
            .Setup(x => x.RecordAsync(It.IsAny<AuditLog>()))
            .Callback<AuditLog>(log => capturedAuditLog = log)
            .Returns(Task.CompletedTask);

        // Act
        await _handler.Handle(notification, CancellationToken.None);

        // Assert
        _mockAuditLogService.Verify(x => x.RecordAsync(It.IsAny<AuditLog>()), Times.Once);
        capturedAuditLog.Should().NotBeNull();
        capturedAuditLog!.EntityType.Should().Be("Journal");
        capturedAuditLog.EntityId.Should().Be("J2024-0001");
        capturedAuditLog.Action.Should().Be(AuditAction.CREATE);
        // AuditLog.Create は Changes プロパティにデータを格納
        capturedAuditLog.Changes.Should().BeEquivalentTo(journalData);
    }

    [Fact]
    public async Task Handle_JournalDeletedEvent_ShouldRecordAuditLog()
    {
        // Arrange
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

        AuditLog? capturedAuditLog = null;
        _mockAuditLogService
            .Setup(x => x.RecordAsync(It.IsAny<AuditLog>()))
            .Callback<AuditLog>(log => capturedAuditLog = log)
            .Returns(Task.CompletedTask);

        // Act
        await _handler.Handle(notification, CancellationToken.None);

        // Assert
        _mockAuditLogService.Verify(x => x.RecordAsync(It.IsAny<AuditLog>()), Times.Once);
        capturedAuditLog.Should().NotBeNull();
        capturedAuditLog!.EntityType.Should().Be("Journal");
        capturedAuditLog.EntityId.Should().Be("J2024-0001");
        capturedAuditLog.Action.Should().Be(AuditAction.DELETE);
        capturedAuditLog.OldValues.Should().BeEquivalentTo(deletedData);
        capturedAuditLog.Reason.Should().Be("入力ミスのため削除");
    }

    [Fact]
    public async Task Handle_WhenIpAddressIsNull_ShouldStillRecordAuditLog()
    {
        // Arrange
        var notification = new AccountCreatedEvent
        {
            AccountCode = "1100",
            AccountData = new Dictionary<string, object>(),
            UserId = "user123",
            UserName = "山田太郎",
            IpAddress = null
        };

        AuditLog? capturedAuditLog = null;
        _mockAuditLogService
            .Setup(x => x.RecordAsync(It.IsAny<AuditLog>()))
            .Callback<AuditLog>(log => capturedAuditLog = log)
            .Returns(Task.CompletedTask);

        // Act
        await _handler.Handle(notification, CancellationToken.None);

        // Assert
        _mockAuditLogService.Verify(x => x.RecordAsync(It.IsAny<AuditLog>()), Times.Once);
        capturedAuditLog.Should().NotBeNull();
        capturedAuditLog!.IpAddress.Should().BeNull();
    }

    [Fact]
    public async Task Handle_ShouldLogInformationMessage()
    {
        // Arrange
        var notification = new AccountCreatedEvent
        {
            AccountCode = "1100",
            AccountData = new Dictionary<string, object>(),
            UserId = "user123",
            UserName = "山田太郎"
        };

        // Act
        await _handler.Handle(notification, CancellationToken.None);

        // Assert
        _mockLogger.Verify(
            x => x.Log(
                LogLevel.Information,
                It.IsAny<EventId>(),
                It.Is<It.IsAnyType>((v, t) => v.ToString()!.Contains("AccountCreatedEvent")),
                It.IsAny<Exception>(),
                It.IsAny<Func<It.IsAnyType, Exception?, string>>()),
            Times.Once);
    }
}
