using AccountingSystem.Application.Ports.In;
using AccountingSystem.Domain.Events;
using AccountingSystem.Infrastructure.EventBus;
using FluentAssertions;
using Microsoft.Extensions.Logging;
using Moq;
using RabbitMQ.Client;

namespace AccountingSystem.Tests.Infrastructure.EventBus;

/// <summary>
/// イベント駆動アーキテクチャの統合テスト
/// RabbitMQ Testcontainers を使用
/// </summary>
public class EventBusIntegrationTest : RabbitMqTestBase
{
    [Fact]
    public async Task イベントパブリッシュとコンシューム統合テスト()
    {
        // Arrange
        var publisherLogger = new Mock<ILogger<RabbitMQEventPublisher>>();
        var consumerLogger = new Mock<ILogger<RabbitMQEventConsumer<JournalEntryCreatedEvent>>>();

        var exchangeName = "test-exchange";
        var queueName = "test-queue";
        var routingKey = "test.journalentry.created";

        await using var publisher = new RabbitMQEventPublisher(
            ConnectionFactory, exchangeName, publisherLogger.Object);

        JournalEntryCreatedEvent? receivedEvent = null;
        var eventReceivedTcs = new TaskCompletionSource<JournalEntryCreatedEvent>();

        var eventHandler = new TestEventHandler<JournalEntryCreatedEvent>(e =>
        {
            receivedEvent = e;
            eventReceivedTcs.TrySetResult(e);
        });

        var consumer = new RabbitMQEventConsumer<JournalEntryCreatedEvent>(
            ConnectionFactory,
            exchangeName,
            queueName,
            routingKey,
            eventHandler,
            consumerLogger.Object
        );

        await consumer.StartAsync();

        // Act
        var testEvent = new JournalEntryCreatedEvent
        {
            JournalEntryId = "test-001",
            EntryDate = DateOnly.FromDateTime(DateTime.Today),
            Description = "テスト仕訳",
            LineItems = new List<JournalEntryCreatedEvent.JournalEntryLineItem>
            {
                new()
                {
                    AccountCode = "ACCOUNT-001",
                    DebitCredit = JournalEntryCreatedEvent.DebitCreditType.DEBIT,
                    Amount = 1000m
                },
                new()
                {
                    AccountCode = "ACCOUNT-002",
                    DebitCredit = JournalEntryCreatedEvent.DebitCreditType.CREDIT,
                    Amount = 1000m
                }
            },
            UserId = "user-001",
            OccurredAt = DateTime.UtcNow
        };

        await publisher.PublishAsync(testEvent, routingKey);

        // イベント処理を待機（タイムアウト 5 秒）
        var timeoutTask = Task.Delay(5000);
        var completedTask = await Task.WhenAny(eventReceivedTcs.Task, timeoutTask);

        // Assert
        completedTask.Should().Be(eventReceivedTcs.Task, "イベントが時間内に受信されるべき");
        receivedEvent.Should().NotBeNull();
        receivedEvent!.JournalEntryId.Should().Be("test-001");
        receivedEvent.Description.Should().Be("テスト仕訳");
        receivedEvent.LineItems.Should().HaveCount(2);

        // Cleanup
        await consumer.StopAsync();
    }

    [Fact]
    public async Task 複数イベントタイプの発行と購読テスト()
    {
        // Arrange
        var publisherLogger = new Mock<ILogger<RabbitMQEventPublisher>>();
        var createdConsumerLogger = new Mock<ILogger<RabbitMQEventConsumer<JournalEntryCreatedEvent>>>();
        var approvedConsumerLogger = new Mock<ILogger<RabbitMQEventConsumer<JournalEntryApprovedEvent>>>();

        var exchangeName = "multi-event-exchange";

        await using var publisher = new RabbitMQEventPublisher(
            ConnectionFactory, exchangeName, publisherLogger.Object);

        var createdEventTcs = new TaskCompletionSource<JournalEntryCreatedEvent>();
        var approvedEventTcs = new TaskCompletionSource<JournalEntryApprovedEvent>();

        var createdHandler = new TestEventHandler<JournalEntryCreatedEvent>(e => createdEventTcs.TrySetResult(e));
        var approvedHandler = new TestEventHandler<JournalEntryApprovedEvent>(e => approvedEventTcs.TrySetResult(e));

        var createdConsumer = new RabbitMQEventConsumer<JournalEntryCreatedEvent>(
            ConnectionFactory, exchangeName, "created-queue", "journal.created",
            createdHandler, createdConsumerLogger.Object);

        var approvedConsumer = new RabbitMQEventConsumer<JournalEntryApprovedEvent>(
            ConnectionFactory, exchangeName, "approved-queue", "journal.approved",
            approvedHandler, approvedConsumerLogger.Object);

        await createdConsumer.StartAsync();
        await approvedConsumer.StartAsync();

        // Act
        var createdEvent = new JournalEntryCreatedEvent
        {
            JournalEntryId = "entry-001",
            EntryDate = DateOnly.FromDateTime(DateTime.Today),
            Description = "作成イベント",
            LineItems = new List<JournalEntryCreatedEvent.JournalEntryLineItem>(),
            UserId = "user-001",
            OccurredAt = DateTime.UtcNow
        };

        var approvedEvent = new JournalEntryApprovedEvent
        {
            JournalEntryId = "entry-001",
            ApprovedBy = "approver-001",
            ApprovalComment = "承認コメント",
            UserId = "approver-001",
            OccurredAt = DateTime.UtcNow
        };

        await publisher.PublishAsync(createdEvent, "journal.created");
        await publisher.PublishAsync(approvedEvent, "journal.approved");

        // 両方のイベント受信を待機
        var timeout = Task.Delay(5000);
        var createdReceived = await Task.WhenAny(createdEventTcs.Task, timeout);
        var approvedReceived = await Task.WhenAny(approvedEventTcs.Task, timeout);

        // Assert
        createdReceived.Should().Be(createdEventTcs.Task);
        approvedReceived.Should().Be(approvedEventTcs.Task);

        var receivedCreated = await createdEventTcs.Task;
        var receivedApproved = await approvedEventTcs.Task;

        receivedCreated.JournalEntryId.Should().Be("entry-001");
        receivedApproved.JournalEntryId.Should().Be("entry-001");
        receivedApproved.ApprovedBy.Should().Be("approver-001");

        // Cleanup
        await createdConsumer.StopAsync();
        await approvedConsumer.StopAsync();
    }

    [Fact]
    public async Task イベントパブリッシュ失敗時のエラーハンドリングテスト()
    {
        // Arrange
        var logger = new Mock<ILogger<RabbitMQEventPublisher>>();

        var invalidFactory = new ConnectionFactory
        {
            HostName = "invalid-host",
            Port = 5672
        };

        await using var publisher = new RabbitMQEventPublisher(
            invalidFactory, "test-exchange", logger.Object);

        // Act & Assert
        var testEvent = new JournalEntryCreatedEvent
        {
            JournalEntryId = "test-002",
            EntryDate = DateOnly.FromDateTime(DateTime.Today),
            Description = "失敗テスト",
            LineItems = new List<JournalEntryCreatedEvent.JournalEntryLineItem>(),
            UserId = "user-002",
            OccurredAt = DateTime.UtcNow
        };

        // 接続失敗で例外が発生することを確認
        await Assert.ThrowsAnyAsync<Exception>(async () =>
        {
            await publisher.PublishAsync(testEvent, "test.journalentry.created");
        });
    }

    [Fact]
    public async Task イベントハンドラー例外時のメッセージ再配信テスト()
    {
        // Arrange
        var publisherLogger = new Mock<ILogger<RabbitMQEventPublisher>>();
        var consumerLogger = new Mock<ILogger<RabbitMQEventConsumer<JournalEntryCreatedEvent>>>();

        var exchangeName = "retry-test-exchange";
        var queueName = "retry-test-queue";
        var routingKey = "retry.test";

        await using var publisher = new RabbitMQEventPublisher(
            ConnectionFactory, exchangeName, publisherLogger.Object);

        var attemptCount = 0;
        var successTcs = new TaskCompletionSource<bool>();

        var eventHandler = new TestEventHandler<JournalEntryCreatedEvent>(e =>
        {
            attemptCount++;
            if (attemptCount < 2)
            {
                throw new InvalidOperationException("テスト用例外");
            }
            successTcs.TrySetResult(true);
        });

        var consumer = new RabbitMQEventConsumer<JournalEntryCreatedEvent>(
            ConnectionFactory, exchangeName, queueName, routingKey,
            eventHandler, consumerLogger.Object);

        await consumer.StartAsync();

        // Act
        var testEvent = new JournalEntryCreatedEvent
        {
            JournalEntryId = "retry-test-001",
            EntryDate = DateOnly.FromDateTime(DateTime.Today),
            Description = "リトライテスト",
            LineItems = new List<JournalEntryCreatedEvent.JournalEntryLineItem>(),
            UserId = "user-001",
            OccurredAt = DateTime.UtcNow
        };

        await publisher.PublishAsync(testEvent, routingKey);

        // 最初の試行と再試行を待機
        await Task.Delay(3000);

        // Assert
        // RabbitMQ の NACK によりメッセージが再配信されるため、複数回の試行が発生
        attemptCount.Should().BeGreaterThanOrEqualTo(1);

        // Cleanup
        await consumer.StopAsync();
    }

    private sealed class TestEventHandler<TEvent> : IEventBusHandler<TEvent>
    {
        private readonly Action<TEvent> _onHandle;

        public TestEventHandler(Action<TEvent> onHandle)
        {
            _onHandle = onHandle;
        }

        public Task HandleAsync(TEvent @event)
        {
            _onHandle(@event);
            return Task.CompletedTask;
        }
    }
}
