using AccountingSystem.Application.Ports.In;
using AccountingSystem.Application.Services;
using AccountingSystem.Domain.Aggregates;
using AccountingSystem.Infrastructure.Persistence.Repositories;
using Dapper;
using FluentAssertions;
using Npgsql;
using Xunit;

namespace AccountingSystem.Tests.Application.Services;

/// <summary>
/// JournalEntryEventSourcingService のテスト
/// </summary>
public class JournalEntryEventSourcingServiceTest : DatabaseTestBase
{
    static JournalEntryEventSourcingServiceTest()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
    }

    private JournalEntryEventSourcingService CreateService()
    {
        var eventStoreRepository = new EventStoreRepository(ConnectionString);
        return new JournalEntryEventSourcingService(eventStoreRepository);
    }

    private static List<LineItemDto> CreateValidLineItems()
    {
        return new List<LineItemDto>
        {
            new() { AccountCode = "1000", DebitCredit = "DEBIT", Amount = 10000m },
            new() { AccountCode = "2000", DebitCredit = "CREDIT", Amount = 10000m }
        };
    }

    [Fact(DisplayName = "仕訳を作成できる")]
    public async Task CreateJournalEntryAsync_仕訳を作成できる()
    {
        // Arrange
        var service = CreateService();
        var entryDate = new DateOnly(2025, 1, 15);
        var description = "テスト仕訳";
        var lineItems = CreateValidLineItems();
        var userId = "user1";

        // Act
        var journalEntryId = await service.CreateJournalEntryAsync(
            entryDate, description, lineItems, userId);

        // Assert
        journalEntryId.Should().NotBeNullOrEmpty();

        // イベントストアに保存されていることを確認
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        var eventCount = await connection.ExecuteScalarAsync<int>(@"
            SELECT COUNT(*) FROM ""イベントストア"" WHERE ""集約ID"" = @Id",
            new { Id = journalEntryId });
        eventCount.Should().Be(1);
    }

    [Fact(DisplayName = "作成した仕訳を取得できる")]
    public async Task GetJournalEntryAsync_作成した仕訳を取得できる()
    {
        // Arrange
        var service = CreateService();
        var entryDate = new DateOnly(2025, 1, 15);
        var description = "取得テスト仕訳";
        var lineItems = CreateValidLineItems();
        var userId = "user1";

        var journalEntryId = await service.CreateJournalEntryAsync(
            entryDate, description, lineItems, userId);

        // Act
        var aggregate = await service.GetJournalEntryAsync(journalEntryId);

        // Assert
        aggregate.Id.Should().Be(journalEntryId);
        aggregate.EntryDate.Should().Be(entryDate);
        aggregate.Description.Should().Be(description);
        aggregate.Status.Should().Be(JournalEntryAggregate.JournalEntryStatus.DRAFT);
        aggregate.Deleted.Should().BeFalse();
        aggregate.LineItems.Should().HaveCount(2);
    }

    [Fact(DisplayName = "存在しない仕訳を取得しようとするとエラー")]
    public async Task GetJournalEntryAsync_存在しない仕訳でエラー()
    {
        // Arrange
        var service = CreateService();

        // Act & Assert
        var act = () => service.GetJournalEntryAsync("non-existent-id");
        await act.Should().ThrowAsync<ArgumentException>()
            .WithMessage("*仕訳が見つかりません*");
    }

    [Fact(DisplayName = "仕訳を承認できる")]
    public async Task ApproveJournalEntryAsync_仕訳を承認できる()
    {
        // Arrange
        var service = CreateService();
        var journalEntryId = await service.CreateJournalEntryAsync(
            new DateOnly(2025, 1, 15),
            "承認テスト仕訳",
            CreateValidLineItems(),
            "user1");

        // Act
        await service.ApproveJournalEntryAsync(journalEntryId, "approver1", "承認します");

        // Assert
        var aggregate = await service.GetJournalEntryAsync(journalEntryId);
        aggregate.Status.Should().Be(JournalEntryAggregate.JournalEntryStatus.APPROVED);

        // イベントが2件保存されていることを確認（作成 + 承認）
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        var eventCount = await connection.ExecuteScalarAsync<int>(@"
            SELECT COUNT(*) FROM ""イベントストア"" WHERE ""集約ID"" = @Id",
            new { Id = journalEntryId });
        eventCount.Should().Be(2);
    }

    [Fact(DisplayName = "既に承認済みの仕訳を承認しようとするとエラー")]
    public async Task ApproveJournalEntryAsync_既に承認済みでエラー()
    {
        // Arrange
        var service = CreateService();
        var journalEntryId = await service.CreateJournalEntryAsync(
            new DateOnly(2025, 1, 15),
            "二重承認テスト仕訳",
            CreateValidLineItems(),
            "user1");

        await service.ApproveJournalEntryAsync(journalEntryId, "approver1", "承認します");

        // Act & Assert
        var act = () => service.ApproveJournalEntryAsync(journalEntryId, "approver2", "再承認");
        await act.Should().ThrowAsync<InvalidOperationException>()
            .WithMessage("すでに承認済みです");
    }

    [Fact(DisplayName = "存在しない仕訳を承認しようとするとエラー")]
    public async Task ApproveJournalEntryAsync_存在しない仕訳でエラー()
    {
        // Arrange
        var service = CreateService();

        // Act & Assert
        var act = () => service.ApproveJournalEntryAsync("non-existent-id", "approver1", "承認");
        await act.Should().ThrowAsync<ArgumentException>()
            .WithMessage("*仕訳が見つかりません*");
    }

    [Fact(DisplayName = "仕訳を削除できる")]
    public async Task DeleteJournalEntryAsync_仕訳を削除できる()
    {
        // Arrange
        var service = CreateService();
        var journalEntryId = await service.CreateJournalEntryAsync(
            new DateOnly(2025, 1, 15),
            "削除テスト仕訳",
            CreateValidLineItems(),
            "user1");

        // Act
        await service.DeleteJournalEntryAsync(journalEntryId, "誤入力のため", "user1");

        // Assert
        var aggregate = await service.GetJournalEntryAsync(journalEntryId);
        aggregate.Deleted.Should().BeTrue();

        // イベントが2件保存されていることを確認（作成 + 削除）
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        var eventCount = await connection.ExecuteScalarAsync<int>(@"
            SELECT COUNT(*) FROM ""イベントストア"" WHERE ""集約ID"" = @Id",
            new { Id = journalEntryId });
        eventCount.Should().Be(2);
    }

    [Fact(DisplayName = "存在しない仕訳を削除しようとするとエラー")]
    public async Task DeleteJournalEntryAsync_存在しない仕訳でエラー()
    {
        // Arrange
        var service = CreateService();

        // Act & Assert
        var act = () => service.DeleteJournalEntryAsync("non-existent-id", "削除理由", "user1");
        await act.Should().ThrowAsync<ArgumentException>()
            .WithMessage("*仕訳が見つかりません*");
    }

    [Fact(DisplayName = "特定時点の仕訳状態を取得できる（タイムトラベル）")]
    public async Task GetJournalEntryAtAsync_特定時点の状態を取得できる()
    {
        // Arrange
        var service = CreateService();
        var journalEntryId = await service.CreateJournalEntryAsync(
            new DateOnly(2025, 1, 15),
            "タイムトラベルテスト仕訳",
            CreateValidLineItems(),
            "user1");

        // 作成直後の時点を記録
        var afterCreation = DateTime.UtcNow;
        await Task.Delay(100); // 時間差を確保

        // 承認
        await service.ApproveJournalEntryAsync(journalEntryId, "approver1", "承認します");

        // Act: 承認前の時点を指定
        var aggregateBeforeApproval = await service.GetJournalEntryAtAsync(
            journalEntryId, afterCreation);

        // Assert: 承認前の状態（DRAFT）
        aggregateBeforeApproval.Status.Should().Be(JournalEntryAggregate.JournalEntryStatus.DRAFT);

        // 現在の状態は APPROVED
        var currentAggregate = await service.GetJournalEntryAsync(journalEntryId);
        currentAggregate.Status.Should().Be(JournalEntryAggregate.JournalEntryStatus.APPROVED);
    }

    [Fact(DisplayName = "指定時点に仕訳が存在しない場合はエラー")]
    public async Task GetJournalEntryAtAsync_指定時点に存在しない場合エラー()
    {
        // Arrange
        var service = CreateService();
        var beforeCreation = DateTime.UtcNow;
        await Task.Delay(100);

        var journalEntryId = await service.CreateJournalEntryAsync(
            new DateOnly(2025, 1, 15),
            "存在確認テスト仕訳",
            CreateValidLineItems(),
            "user1");

        // Act & Assert: 作成前の時点を指定
        var act = () => service.GetJournalEntryAtAsync(journalEntryId, beforeCreation);
        await act.Should().ThrowAsync<ArgumentException>()
            .WithMessage("*指定時点の仕訳が見つかりません*");
    }

    [Fact(DisplayName = "借方貸方が一致しない仕訳は作成できない")]
    public async Task CreateJournalEntryAsync_借方貸方不一致でエラー()
    {
        // Arrange
        var service = CreateService();
        var unbalancedLineItems = new List<LineItemDto>
        {
            new() { AccountCode = "1000", DebitCredit = "DEBIT", Amount = 10000m },
            new() { AccountCode = "2000", DebitCredit = "CREDIT", Amount = 5000m } // 金額不一致
        };

        // Act & Assert
        var act = () => service.CreateJournalEntryAsync(
            new DateOnly(2025, 1, 15),
            "不正な仕訳",
            unbalancedLineItems,
            "user1");

        await act.Should().ThrowAsync<ArgumentException>()
            .WithMessage("*借方合計と貸方合計が一致しません*");
    }

    [Fact(DisplayName = "複数の操作を順番に実行できる")]
    public async Task MultipleOperations_順番に実行できる()
    {
        // Arrange
        var service = CreateService();

        // Act: 作成 → 承認 → 削除
        var journalEntryId = await service.CreateJournalEntryAsync(
            new DateOnly(2025, 1, 15),
            "複合操作テスト仕訳",
            CreateValidLineItems(),
            "user1");

        await service.ApproveJournalEntryAsync(journalEntryId, "approver1", "承認します");
        await service.DeleteJournalEntryAsync(journalEntryId, "取り消しのため", "user1");

        // Assert
        var aggregate = await service.GetJournalEntryAsync(journalEntryId);
        aggregate.Status.Should().Be(JournalEntryAggregate.JournalEntryStatus.APPROVED);
        aggregate.Deleted.Should().BeTrue();
        aggregate.Version.Should().Be(3); // 作成 + 承認 + 削除

        // イベントが3件保存されていることを確認
        await using var connection = new NpgsqlConnection(ConnectionString);
        await connection.OpenAsync();
        var eventCount = await connection.ExecuteScalarAsync<int>(@"
            SELECT COUNT(*) FROM ""イベントストア"" WHERE ""集約ID"" = @Id",
            new { Id = journalEntryId });
        eventCount.Should().Be(3);
    }
}
