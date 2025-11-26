using AccountingSystem.Infrastructure.Persistence.Dapper.Entities;
using AccountingSystem.Infrastructure.Persistence.Repositories;
using FluentAssertions;
using Testcontainers.PostgreSql;
using Xunit;

namespace AccountingSystem.Tests.Repositories;

/// <summary>
/// 課税取引マスタ - Dapper 統合テスト
/// </summary>
public class TaxTransactionRepositoryTest : IAsyncLifetime
{
    private readonly PostgreSqlContainer _postgres;
    private TestDatabase? _testDb;
    private TaxTransactionRepository? _repository;

    public TaxTransactionRepositoryTest()
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
        _repository = new TaxTransactionRepository(_postgres.GetConnectionString());
    }

    public async Task DisposeAsync()
    {
        await _testDb!.StopAsync();
        await _postgres.DisposeAsync();
    }

    [Fact(DisplayName = "初期データが投入されている")]
    public async Task TestInitialData()
    {
        var all = await _repository!.FindAllAsync();
        all.Should().HaveCount(4);

        var codes = all.Select(t => t.TaxCode).ToList();
        codes.Should().Contain(new[] { "01", "02", "03", "04" });
    }

    [Fact(DisplayName = "課税取引コードで検索できる")]
    public async Task TestFindByCode()
    {
        var taxable = await _repository!.FindByCodeAsync("01");

        taxable.Should().NotBeNull();
        taxable!.TaxCode.Should().Be("01");
        taxable.TaxName.Should().Be("課税");
        taxable.TaxRate.Should().Be(0.10m);
    }

    [Fact(DisplayName = "有効な課税取引のみ取得できる")]
    public async Task TestFindActive()
    {
        // すべて有効なので4件取得される
        var active = await _repository!.FindActiveAsync();
        active.Should().HaveCount(4);
    }

    [Fact(DisplayName = "新しい課税取引を登録できる")]
    public async Task TestInsert()
    {
        var newTax = new TaxTransaction
        {
            TaxCode = "05",
            TaxName = "軽減税率",
            TaxRate = 0.08m,
            Description = "食料品等に適用される軽減税率",
            IsActive = true
        };

        await _repository!.InsertAsync(newTax);

        var found = await _repository.FindByCodeAsync("05");
        found.Should().NotBeNull();
        found!.TaxName.Should().Be("軽減税率");
        found.TaxRate.Should().Be(0.08m);
    }

    [Fact(DisplayName = "課税取引を更新できる")]
    public async Task TestUpdate()
    {
        // 既存の課税取引を取得
        var existing = await _repository!.FindByCodeAsync("01");
        existing.Should().NotBeNull();

        // 更新
        var updated = existing! with { TaxRate = 0.12m, Description = "更新後の説明" };
        await _repository.UpdateAsync(updated);

        // 検証
        var found = await _repository.FindByCodeAsync("01");
        found!.TaxRate.Should().Be(0.12m);
        found.Description.Should().Be("更新後の説明");
    }

    [Fact(DisplayName = "課税取引を削除できる")]
    public async Task TestDelete()
    {
        // 新しいデータを登録してから削除
        var newTax = new TaxTransaction
        {
            TaxCode = "99",
            TaxName = "テスト用",
            TaxRate = 0.00m,
            IsActive = true
        };
        await _repository!.InsertAsync(newTax);

        await _repository.DeleteAsync("99");

        var deleted = await _repository.FindByCodeAsync("99");
        deleted.Should().BeNull();
    }
}
