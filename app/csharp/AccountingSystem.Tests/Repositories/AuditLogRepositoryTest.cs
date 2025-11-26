using AccountingSystem.Domain.Audit;
using AccountingSystem.Infrastructure.Persistence.Dapper.Entities;
using AccountingSystem.Infrastructure.Persistence.Repositories;
using FluentAssertions;
using Xunit;

namespace AccountingSystem.Tests.Repositories;

/// <summary>
/// AuditLogRepository の統合テスト
/// </summary>
public class AuditLogRepositoryTest : DatabaseTestBase
{
    [Fact]
    public async Task InsertAsync_ShouldInsertAuditLogAndReturnWithId()
    {
        // Arrange
        var repository = new AuditLogRepository(ConnectionString);
        var auditLog = AuditLog.Create(
            "Account",
            "1100",
            AuditAction.CREATE,
            "user123",
            "山田太郎",
            new Dictionary<string, object>
            {
                ["code"] = "1100",
                ["name"] = "現金"
            },
            "192.168.1.1"
        );

        // Act
        var result = await repository.InsertAsync(auditLog);

        // Assert
        result.Should().NotBeNull();
        result.Id.Should().BeGreaterThan(0);
        result.EntityType.Should().Be("Account");
        result.EntityId.Should().Be("1100");
        result.Action.Should().Be(AuditAction.CREATE);
        result.UserId.Should().Be("user123");
        result.UserName.Should().Be("山田太郎");
    }

    [Fact]
    public async Task FindByEntityAsync_ShouldReturnAuditLogsForEntity()
    {
        // Arrange
        var repository = new AuditLogRepository(ConnectionString);

        // 複数の監査ログを登録
        var log1 = AuditLog.Create("Account", "1100", AuditAction.CREATE, "user1", "ユーザー1",
            new Dictionary<string, object> { ["name"] = "現金" }, null);
        var log2 = AuditLog.CreateForUpdate("Account", "1100", "user2", "ユーザー2",
            new Dictionary<string, object> { ["name"] = "現金" },
            new Dictionary<string, object> { ["name"] = "小口現金" }, null);
        var log3 = AuditLog.Create("Account", "1200", AuditAction.CREATE, "user1", "ユーザー1",
            new Dictionary<string, object> { ["name"] = "普通預金" }, null);

        await repository.InsertAsync(log1);
        await repository.InsertAsync(log2);
        await repository.InsertAsync(log3);

        // Act
        var results = await repository.FindByEntityAsync("Account", "1100");

        // Assert
        results.Should().HaveCount(2);
        results.Should().AllSatisfy(log => log.EntityId.Should().Be("1100"));
        results.First().Action.Should().Be(AuditAction.UPDATE); // 最新順
        results.Last().Action.Should().Be(AuditAction.CREATE);
    }

    [Fact]
    public async Task FindByUserAsync_ShouldReturnAuditLogsForUser()
    {
        // Arrange
        var repository = new AuditLogRepository(ConnectionString);
        var now = DateTime.UtcNow;

        var log1 = AuditLog.Create("Account", "1100", AuditAction.CREATE, "user123", "山田太郎",
            new Dictionary<string, object>(), null);
        var log2 = AuditLog.Create("Journal", "J2024-0001", AuditAction.CREATE, "user123", "山田太郎",
            new Dictionary<string, object>(), null);
        var log3 = AuditLog.Create("Account", "1200", AuditAction.CREATE, "user456", "佐藤次郎",
            new Dictionary<string, object>(), null);

        await repository.InsertAsync(log1);
        await repository.InsertAsync(log2);
        await repository.InsertAsync(log3);

        // Act
        var results = await repository.FindByUserAsync("user123", now.AddMinutes(-1), now.AddMinutes(1));

        // Assert
        results.Should().HaveCount(2);
        results.Should().AllSatisfy(log => log.UserId.Should().Be("user123"));
    }

    [Fact]
    public async Task FindByPeriodAsync_ShouldReturnAuditLogsWithinPeriod()
    {
        // Arrange
        var repository = new AuditLogRepository(ConnectionString);
        var now = DateTime.UtcNow;

        // 複数ログを登録
        for (int i = 0; i < 5; i++)
        {
            var log = AuditLog.Create("Account", $"110{i}", AuditAction.CREATE, "user1", "ユーザー1",
                new Dictionary<string, object>(), null);
            await repository.InsertAsync(log);
        }

        // Act
        var results = await repository.FindByPeriodAsync(now.AddMinutes(-1), now.AddMinutes(1), 3);

        // Assert
        results.Should().HaveCount(3); // limit=3
    }

    [Fact]
    public async Task FindByActionAsync_ShouldReturnAuditLogsByAction()
    {
        // Arrange
        var repository = new AuditLogRepository(ConnectionString);
        var now = DateTime.UtcNow;

        var createLog = AuditLog.Create("Account", "1100", AuditAction.CREATE, "user1", "ユーザー1",
            new Dictionary<string, object>(), null);
        var updateLog = AuditLog.CreateForUpdate("Account", "1100", "user2", "ユーザー2",
            new Dictionary<string, object>(), new Dictionary<string, object>(), null);
        var deleteLog = AuditLog.CreateForDelete("Account", "1200", "user1", "ユーザー1",
            new Dictionary<string, object>(), "テスト削除", null);

        await repository.InsertAsync(createLog);
        await repository.InsertAsync(updateLog);
        await repository.InsertAsync(deleteLog);

        // Act
        var createResults = await repository.FindByActionAsync(AuditAction.CREATE, now.AddMinutes(-1), now.AddMinutes(1), 100);
        var updateResults = await repository.FindByActionAsync(AuditAction.UPDATE, now.AddMinutes(-1), now.AddMinutes(1), 100);
        var deleteResults = await repository.FindByActionAsync(AuditAction.DELETE, now.AddMinutes(-1), now.AddMinutes(1), 100);

        // Assert
        createResults.Should().HaveCount(1);
        updateResults.Should().HaveCount(1);
        deleteResults.Should().HaveCount(1);
    }

    [Fact]
    public async Task InsertAsync_ShouldPersistJsonData()
    {
        // Arrange
        var repository = new AuditLogRepository(ConnectionString);
        var oldValues = new Dictionary<string, object>
        {
            ["name"] = "現金",
            ["balance"] = 100000
        };
        var newValues = new Dictionary<string, object>
        {
            ["name"] = "小口現金",
            ["balance"] = 50000
        };

        var auditLog = AuditLog.CreateForUpdate(
            "Account", "1100", "user1", "ユーザー1",
            oldValues, newValues, "192.168.1.1"
        );

        // Act
        await repository.InsertAsync(auditLog);
        var results = await repository.FindByEntityAsync("Account", "1100");

        // Assert
        results.Should().HaveCount(1);
        var result = results.First();
        result.OldValues.Should().NotBeNull();
        result.NewValues.Should().NotBeNull();
    }

    [Fact]
    public async Task InsertAsync_ShouldPersistReason()
    {
        // Arrange
        var repository = new AuditLogRepository(ConnectionString);
        var auditLog = AuditLog.CreateForDelete(
            "Account", "1100", "user1", "ユーザー1",
            new Dictionary<string, object> { ["name"] = "現金" },
            "重複登録のため削除",
            "192.168.1.1"
        );

        // Act
        await repository.InsertAsync(auditLog);
        var results = await repository.FindByEntityAsync("Account", "1100");

        // Assert
        results.Should().HaveCount(1);
        results.First().Reason.Should().Be("重複登録のため削除");
    }

    [Fact]
    public async Task FindByEntityAsync_ShouldReturnEmptyListWhenNoLogs()
    {
        // Arrange
        var repository = new AuditLogRepository(ConnectionString);

        // Act
        var results = await repository.FindByEntityAsync("NonExistent", "000");

        // Assert
        results.Should().BeEmpty();
    }

    [Fact]
    public async Task FindByUserAsync_ShouldReturnEmptyListWhenNoLogs()
    {
        // Arrange
        var repository = new AuditLogRepository(ConnectionString);
        var now = DateTime.UtcNow;

        // Act
        var results = await repository.FindByUserAsync("nonexistent", now.AddDays(-1), now);

        // Assert
        results.Should().BeEmpty();
    }

    [Fact]
    public async Task FindByEntityAsync_ShouldReturnLogsInDescendingOrder()
    {
        // Arrange
        var repository = new AuditLogRepository(ConnectionString);

        var log1 = AuditLog.Create("Account", "1100", AuditAction.CREATE, "user1", "ユーザー1",
            new Dictionary<string, object>(), null);
        await repository.InsertAsync(log1);
        await Task.Delay(10); // 時間差を確保

        var log2 = AuditLog.CreateForUpdate("Account", "1100", "user1", "ユーザー1",
            new Dictionary<string, object>(), new Dictionary<string, object>(), null);
        await repository.InsertAsync(log2);
        await Task.Delay(10);

        var log3 = AuditLog.CreateForDelete("Account", "1100", "user1", "ユーザー1",
            new Dictionary<string, object>(), "削除", null);
        await repository.InsertAsync(log3);

        // Act
        var results = await repository.FindByEntityAsync("Account", "1100");

        // Assert
        results.Should().HaveCount(3);
        results[0].Action.Should().Be(AuditAction.DELETE);  // 最新
        results[1].Action.Should().Be(AuditAction.UPDATE);
        results[2].Action.Should().Be(AuditAction.CREATE);  // 最古
    }
}
