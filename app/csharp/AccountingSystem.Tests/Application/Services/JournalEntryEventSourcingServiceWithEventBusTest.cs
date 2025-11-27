using AccountingSystem.Application.Ports.In;
using AccountingSystem.Application.Ports.Out;
using AccountingSystem.Application.Services;
using AccountingSystem.Domain.Aggregates;
using AccountingSystem.Domain.Events;
using FluentAssertions;
using Microsoft.Extensions.Logging;
using Moq;
using static AccountingSystem.Domain.Aggregates.JournalEntryAggregate;

namespace AccountingSystem.Tests.Application.Services;

/// <summary>
/// JournalEntryEventSourcingServiceWithEventBus のテスト
/// イベントバス連携デコレーターの動作を検証
/// </summary>
public class JournalEntryEventSourcingServiceWithEventBusTest
{
    private readonly Mock<IJournalEntryEventSourcingService> _innerServiceMock;
    private readonly Mock<IEventPublisher> _eventPublisherMock;
    private readonly Mock<ILogger<JournalEntryEventSourcingServiceWithEventBus>> _loggerMock;
    private readonly JournalEntryEventSourcingServiceWithEventBus _sut;

    public JournalEntryEventSourcingServiceWithEventBusTest()
    {
        _innerServiceMock = new Mock<IJournalEntryEventSourcingService>();
        _eventPublisherMock = new Mock<IEventPublisher>();
        _loggerMock = new Mock<ILogger<JournalEntryEventSourcingServiceWithEventBus>>();

        _sut = new JournalEntryEventSourcingServiceWithEventBus(
            _innerServiceMock.Object,
            _eventPublisherMock.Object,
            _loggerMock.Object
        );
    }

    [Fact]
    public async Task CreateJournalEntryAsync_成功時にイベントが発行される()
    {
        // Arrange
        var entryDate = DateOnly.FromDateTime(DateTime.Today);
        var description = "テスト仕訳";
        var userId = "user-001";
        var lineItems = new List<LineItemDto>
        {
            new() { AccountCode = "ACCOUNT-001", DebitCredit = "DEBIT", Amount = 1000m },
            new() { AccountCode = "ACCOUNT-002", DebitCredit = "CREDIT", Amount = 1000m }
        };

        var expectedJournalEntryId = "entry-001";

        _innerServiceMock
            .Setup(s => s.CreateJournalEntryAsync(entryDate, description, lineItems, userId))
            .ReturnsAsync(expectedJournalEntryId);

        // Act
        var result = await _sut.CreateJournalEntryAsync(entryDate, description, lineItems, userId);

        // Assert
        result.Should().Be(expectedJournalEntryId);

        _eventPublisherMock.Verify(
            p => p.PublishAsync(
                It.Is<JournalEntryCreatedEvent>(e =>
                    e.JournalEntryId == "entry-001" &&
                    e.Description == description &&
                    e.UserId == userId),
                "financial.journalentry.created"
            ),
            Times.Once
        );
    }

    [Fact]
    public async Task CreateJournalEntryAsync_イベント発行失敗時もメイン処理は成功する()
    {
        // Arrange
        var entryDate = DateOnly.FromDateTime(DateTime.Today);
        var description = "テスト仕訳";
        var userId = "user-001";
        var lineItems = new List<LineItemDto>();

        var expectedJournalEntryId = "entry-002";

        _innerServiceMock
            .Setup(s => s.CreateJournalEntryAsync(entryDate, description, lineItems, userId))
            .ReturnsAsync(expectedJournalEntryId);

        _eventPublisherMock
            .Setup(p => p.PublishAsync(It.IsAny<JournalEntryCreatedEvent>(), It.IsAny<string>()))
            .ThrowsAsync(new Exception("RabbitMQ 接続エラー"));

        // Act
        var result = await _sut.CreateJournalEntryAsync(entryDate, description, lineItems, userId);

        // Assert
        result.Should().Be(expectedJournalEntryId);
        // イベント発行は失敗しても、メイン処理の結果は返される
    }

    [Fact]
    public async Task ApproveJournalEntryAsync_成功時にイベントが発行される()
    {
        // Arrange
        var journalEntryId = "entry-001";
        var approvedBy = "approver-001";
        var comment = "承認コメント";

        _innerServiceMock
            .Setup(s => s.ApproveJournalEntryAsync(journalEntryId, approvedBy, comment))
            .Returns(Task.CompletedTask);

        // Act
        await _sut.ApproveJournalEntryAsync(journalEntryId, approvedBy, comment);

        // Assert
        _eventPublisherMock.Verify(
            p => p.PublishAsync(
                It.Is<JournalEntryApprovedEvent>(e =>
                    e.JournalEntryId == journalEntryId &&
                    e.ApprovedBy == approvedBy &&
                    e.ApprovalComment == comment),
                "financial.journalentry.approved"
            ),
            Times.Once
        );
    }

    [Fact]
    public async Task DeleteJournalEntryAsync_成功時にイベントが発行される()
    {
        // Arrange
        var journalEntryId = "entry-001";
        var reason = "誤入力のため削除";
        var userId = "user-001";

        _innerServiceMock
            .Setup(s => s.DeleteJournalEntryAsync(journalEntryId, reason, userId))
            .Returns(Task.CompletedTask);

        // Act
        await _sut.DeleteJournalEntryAsync(journalEntryId, reason, userId);

        // Assert
        _eventPublisherMock.Verify(
            p => p.PublishAsync(
                It.Is<JournalEntryDeletedEvent>(e =>
                    e.JournalEntryId == journalEntryId &&
                    e.UserId == userId &&
                    e.Reason == reason),
                "financial.journalentry.deleted"
            ),
            Times.Once
        );
    }

    [Fact]
    public async Task GetJournalEntryAsync_イベントは発行されない()
    {
        // Arrange
        var journalEntryId = "entry-001";
        var aggregate = JournalEntryAggregate.Create(
            journalEntryId,
            DateOnly.FromDateTime(DateTime.Today),
            "テスト仕訳",
            new List<JournalEntryAggregate.LineItem>
            {
                new("ACCOUNT-001", DebitCredit.DEBIT, 1000m),
                new("ACCOUNT-002", DebitCredit.CREDIT, 1000m)
            },
            "user-001"
        );

        _innerServiceMock
            .Setup(s => s.GetJournalEntryAsync(journalEntryId))
            .ReturnsAsync(aggregate);

        // Act
        var result = await _sut.GetJournalEntryAsync(journalEntryId);

        // Assert
        result.Should().Be(aggregate);

        // 読み取り操作ではイベントは発行されない
        _eventPublisherMock.Verify(
            p => p.PublishAsync(It.IsAny<object>(), It.IsAny<string>()),
            Times.Never
        );
    }

    [Fact]
    public async Task GetJournalEntryAtAsync_タイムトラベルクエリでイベントは発行されない()
    {
        // Arrange
        var journalEntryId = "entry-001";
        var pointInTime = DateTime.UtcNow.AddDays(-1);
        var aggregate = JournalEntryAggregate.Create(
            journalEntryId,
            DateOnly.FromDateTime(DateTime.Today),
            "テスト仕訳",
            new List<JournalEntryAggregate.LineItem>
            {
                new("ACCOUNT-001", DebitCredit.DEBIT, 1000m),
                new("ACCOUNT-002", DebitCredit.CREDIT, 1000m)
            },
            "user-001"
        );

        _innerServiceMock
            .Setup(s => s.GetJournalEntryAtAsync(journalEntryId, pointInTime))
            .ReturnsAsync(aggregate);

        // Act
        var result = await _sut.GetJournalEntryAtAsync(journalEntryId, pointInTime);

        // Assert
        result.Should().Be(aggregate);

        // 読み取り操作ではイベントは発行されない
        _eventPublisherMock.Verify(
            p => p.PublishAsync(It.IsAny<object>(), It.IsAny<string>()),
            Times.Never
        );
    }

    [Fact]
    public async Task ApproveJournalEntryAsync_イベント発行失敗時もメイン処理は継続する()
    {
        // Arrange
        var journalEntryId = "entry-001";
        var approvedBy = "approver-001";
        var comment = "承認";

        _innerServiceMock
            .Setup(s => s.ApproveJournalEntryAsync(journalEntryId, approvedBy, comment))
            .Returns(Task.CompletedTask);

        _eventPublisherMock
            .Setup(p => p.PublishAsync(It.IsAny<JournalEntryApprovedEvent>(), It.IsAny<string>()))
            .ThrowsAsync(new Exception("接続エラー"));

        // Act & Assert - 例外が発生しないことを確認
        await _sut.Invoking(s => s.ApproveJournalEntryAsync(journalEntryId, approvedBy, comment))
            .Should()
            .NotThrowAsync();
    }
}
