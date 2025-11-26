using AccountingSystem.Domain.Entities;
using AccountingSystem.Infrastructure.Persistence.Repositories;
using FluentAssertions;
using Npgsql;
using Testcontainers.PostgreSql;
using Xunit;

namespace AccountingSystem.Tests.Infrastructure.Repositories;

/// <summary>
/// 勘定科目リポジトリ - Dapper 統合テスト
/// </summary>
public class AccountRepositoryTest : IAsyncLifetime
{
    private readonly PostgreSqlContainer _postgres;
    private TestDatabase? _testDb;
    private AccountRepository? _repository;

    public AccountRepositoryTest()
    {
        _postgres = new PostgreSqlBuilder()
            .WithImage("postgres:16-alpine")
            .WithDatabase("testdb")
            .WithUsername("testuser")
            .WithPassword("testpass")
            .Build();
    }

    public async Task InitializeAsync()
    {
        await _postgres.StartAsync();
        _testDb = new TestDatabase(_postgres);
        await _testDb.StartAsync();
        _repository = new AccountRepository(_postgres.GetConnectionString());
    }

    public async Task DisposeAsync()
    {
        await _testDb!.StopAsync();
        await _postgres.DisposeAsync();
    }

    private async Task CleanupAsync()
    {
        await using var conn = new NpgsqlConnection(_postgres.GetConnectionString());
        await conn.OpenAsync();

        await using var cmd = new NpgsqlCommand(@"TRUNCATE TABLE ""勘定科目マスタ"" CASCADE", conn);
        await cmd.ExecuteNonQueryAsync();
    }

    [Fact(DisplayName = "勘定科目を登録できる")]
    public async Task TestInsert()
    {
        await CleanupAsync();

        var account = new Account("1000", "現金", "資産", false)
        {
            Balance = 100000.00m
        };

        var accountId = await _repository!.InsertAsync(account);
        accountId.Should().BeGreaterThan(0);

        var found = await _repository.FindByCodeAsync("1000");
        found.Should().NotBeNull();
        found!.AccountName.Should().Be("現金");
        found.AccountType.Should().Be("資産");
        found.Balance.Should().Be(100000.00m);
    }

    [Fact(DisplayName = "全ての勘定科目を取得できる")]
    public async Task TestFindAll()
    {
        await CleanupAsync();

        await _repository!.InsertAsync(new Account("1000", "現金", "資産", false));
        await _repository.InsertAsync(new Account("2000", "買掛金", "負債", false));
        await _repository.InsertAsync(new Account("3000", "資本金", "純資産", false));

        var all = await _repository.FindAllAsync();
        all.Should().HaveCount(3);
        all.Select(a => a.AccountCode).Should().ContainInOrder("1000", "2000", "3000");
    }

    [Fact(DisplayName = "勘定科目種別でフィルタリングできる")]
    public async Task TestFindByType()
    {
        await CleanupAsync();

        await _repository!.InsertAsync(new Account("1000", "現金", "資産", false));
        await _repository.InsertAsync(new Account("1100", "普通預金", "資産", false));
        await _repository.InsertAsync(new Account("2000", "買掛金", "負債", false));

        var assets = await _repository.FindByTypeAsync("資産");
        assets.Should().HaveCount(2);
        assets.Select(a => a.AccountName).Should().Contain(new[] { "現金", "普通預金" });
    }

    [Fact(DisplayName = "合計科目と明細科目を区別できる")]
    public async Task TestSummaryAndDetailAccounts()
    {
        await CleanupAsync();

        // 合計科目
        var summary1 = new Account("11", "資産の部", "資産", true);
        var summary2 = new Account("11000", "流動資産", "資産", true);

        // 明細科目
        var detail1 = new Account("11110", "現金", "資産", false);
        var detail2 = new Account("11120", "普通預金", "資産", false);

        await _repository!.InsertAsync(summary1);
        await _repository.InsertAsync(summary2);
        await _repository.InsertAsync(detail1);
        await _repository.InsertAsync(detail2);

        // 合計科目のみ取得
        var summaryAccounts = await _repository.FindSummaryAccountsAsync();
        summaryAccounts.Should().HaveCount(2);
        summaryAccounts.Should().OnlyContain(a => a.IsSummaryAccount);

        // 明細科目のみ取得
        var detailAccounts = await _repository.FindDetailAccountsAsync();
        detailAccounts.Should().HaveCount(2);
        detailAccounts.Should().OnlyContain(a => !a.IsSummaryAccount);
    }

    [Fact(DisplayName = "残高を更新できる")]
    public async Task TestUpdateBalance()
    {
        await CleanupAsync();

        var account = new Account("1000", "現金", "資産", false)
        {
            Balance = 50000.00m
        };
        await _repository!.InsertAsync(account);

        // 残高を更新
        await _repository.UpdateBalanceAsync("1000", 75000.00m);

        var updated = await _repository.FindByCodeAsync("1000");
        updated!.Balance.Should().Be(75000.00m);
    }

    [Fact(DisplayName = "勘定科目を削除できる")]
    public async Task TestDelete()
    {
        await CleanupAsync();

        await _repository!.InsertAsync(new Account("1000", "現金", "資産", false));

        await _repository.DeleteAsync("1000");

        var deleted = await _repository.FindByCodeAsync("1000");
        deleted.Should().BeNull();
    }
}
