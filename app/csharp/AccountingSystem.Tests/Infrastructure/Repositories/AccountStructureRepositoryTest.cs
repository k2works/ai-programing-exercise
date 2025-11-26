using AccountingSystem.Domain.Entities;
using AccountingSystem.Infrastructure.Persistence.Repositories;
using FluentAssertions;
using Npgsql;
using Xunit;

namespace AccountingSystem.Tests.Infrastructure.Repositories;

/// <summary>
/// 勘定科目構成マスタ - Dapper 統合テスト
/// </summary>
public class AccountStructureRepositoryTest : DatabaseTestBase
{
    private AccountStructureRepository CreateRepository() => new AccountStructureRepository(ConnectionString);

    protected override async Task OnInitializedAsync()
    {
        // 勘定科目マスタにテストデータを投入
        await using var conn = new NpgsqlConnection(ConnectionString);
        await conn.OpenAsync();
        await using var cmd = new NpgsqlCommand(@"
            INSERT INTO ""勘定科目マスタ"" (
                ""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""合計科目"", ""集計対象"", ""残高""
            ) VALUES
                ('11', '資産の部', '資産'::account_type, true, true, 0),
                ('11000', '流動資産', '資産'::account_type, true, true, 0),
                ('11190', '現金及び預金', '資産'::account_type, true, true, 0),
                ('11110', '現金', '資産'::account_type, false, true, 100000),
                ('11120', '当座預金', '資産'::account_type, false, true, 500000),
                ('11130', '普通預金', '資産'::account_type, false, true, 1000000)
            ON CONFLICT (""勘定科目コード"") DO NOTHING
        ", conn);
        await cmd.ExecuteNonQueryAsync();
    }

    [Fact(DisplayName = "勘定科目構成を登録できる")]
    public async Task TestInsert()
    {
        var repository = CreateRepository();

        // 階層構造を登録
        var root = new AccountStructure
        {
            AccountCode = "11",
            AccountPath = "11",
            HierarchyLevel = 1,
            ParentAccountCode = null,
            DisplayOrder = 1
        };
        await repository.InsertAsync(root);

        var level2 = new AccountStructure
        {
            AccountCode = "11000",
            AccountPath = "11~11000",
            HierarchyLevel = 2,
            ParentAccountCode = "11",
            DisplayOrder = 1
        };
        await repository.InsertAsync(level2);

        var level3 = new AccountStructure
        {
            AccountCode = "11190",
            AccountPath = "11~11000~11190",
            HierarchyLevel = 3,
            ParentAccountCode = "11000",
            DisplayOrder = 1
        };
        await repository.InsertAsync(level3);

        // 検証
        var found = await repository.FindByCodeAsync("11190");
        found.Should().NotBeNull();
        found!.AccountPath.Should().Be("11~11000~11190");
        found.HierarchyLevel.Should().Be(3);
        found.ParentAccountCode.Should().Be("11000");
    }

    [Fact(DisplayName = "全ての勘定科目構成を取得できる")]
    public async Task TestFindAll()
    {
        var repository = CreateRepository();

        // テストデータ登録
        await repository.InsertAsync(new AccountStructure
        {
            AccountCode = "11", AccountPath = "11", HierarchyLevel = 1, DisplayOrder = 1
        });
        await repository.InsertAsync(new AccountStructure
        {
            AccountCode = "11000", AccountPath = "11~11000", HierarchyLevel = 2, ParentAccountCode = "11", DisplayOrder = 1
        });
        await repository.InsertAsync(new AccountStructure
        {
            AccountCode = "11190", AccountPath = "11~11000~11190", HierarchyLevel = 3, ParentAccountCode = "11000", DisplayOrder = 1
        });

        // 全件取得
        var all = await repository.FindAllAsync();
        all.Should().HaveCount(3);
        all.Select(a => a.AccountCode).Should().ContainInOrder("11", "11000", "11190");
    }

    [Fact(DisplayName = "特定科目配下の子孫を取得できる（チルダ連結検索）")]
    public async Task TestFindChildren()
    {
        var repository = CreateRepository();

        // 階層データ登録
        await repository.InsertAsync(new AccountStructure { AccountCode = "11", AccountPath = "11", HierarchyLevel = 1, DisplayOrder = 1 });
        await repository.InsertAsync(new AccountStructure { AccountCode = "11000", AccountPath = "11~11000", HierarchyLevel = 2, ParentAccountCode = "11", DisplayOrder = 1 });
        await repository.InsertAsync(new AccountStructure { AccountCode = "11190", AccountPath = "11~11000~11190", HierarchyLevel = 3, ParentAccountCode = "11000", DisplayOrder = 1 });
        await repository.InsertAsync(new AccountStructure { AccountCode = "11110", AccountPath = "11~11000~11190~11110", HierarchyLevel = 4, ParentAccountCode = "11190", DisplayOrder = 1 });
        await repository.InsertAsync(new AccountStructure { AccountCode = "11120", AccountPath = "11~11000~11190~11120", HierarchyLevel = 4, ParentAccountCode = "11190", DisplayOrder = 2 });
        await repository.InsertAsync(new AccountStructure { AccountCode = "11130", AccountPath = "11~11000~11190~11130", HierarchyLevel = 4, ParentAccountCode = "11190", DisplayOrder = 3 });

        // 「現金及び預金」（11190）配下を検索
        var children = await repository.FindChildrenAsync("11190");

        // 自身 + 子孫の4件が取得される
        children.Should().HaveCount(4);
        children.Select(c => c.AccountCode).Should().Contain(new[] { "11190", "11110", "11120", "11130" });
    }

    [Fact(DisplayName = "特定階層レベルの科目を取得できる")]
    public async Task TestFindByLevel()
    {
        var repository = CreateRepository();

        await repository.InsertAsync(new AccountStructure { AccountCode = "11", AccountPath = "11", HierarchyLevel = 1, DisplayOrder = 1 });
        await repository.InsertAsync(new AccountStructure { AccountCode = "11000", AccountPath = "11~11000", HierarchyLevel = 2, ParentAccountCode = "11", DisplayOrder = 1 });
        await repository.InsertAsync(new AccountStructure { AccountCode = "11190", AccountPath = "11~11000~11190", HierarchyLevel = 3, ParentAccountCode = "11000", DisplayOrder = 1 });
        await repository.InsertAsync(new AccountStructure { AccountCode = "11110", AccountPath = "11~11000~11190~11110", HierarchyLevel = 4, ParentAccountCode = "11190", DisplayOrder = 1 });

        // 階層レベル4の科目を検索
        var level4 = await repository.FindByLevelAsync(4);
        level4.Should().HaveCount(1);
        level4.First().AccountCode.Should().Be("11110");
    }

    [Fact(DisplayName = "勘定科目構成を更新できる")]
    public async Task TestUpdate()
    {
        var repository = CreateRepository();

        // 初期データ
        var structure = new AccountStructure
        {
            AccountCode = "11110",
            AccountPath = "11~11000~11190~11110",
            HierarchyLevel = 4,
            ParentAccountCode = "11190",
            DisplayOrder = 1
        };
        await repository.InsertAsync(structure);

        // 更新（表示順序を変更）
        var updatedStructure = structure with { DisplayOrder = 99 };
        await repository.UpdateAsync(updatedStructure);

        // 検証
        var updated = await repository.FindByCodeAsync("11110");
        updated!.DisplayOrder.Should().Be(99);
    }

    [Fact(DisplayName = "勘定科目構成を削除できる")]
    public async Task TestDelete()
    {
        var repository = CreateRepository();

        await repository.InsertAsync(new AccountStructure
        {
            AccountCode = "11110",
            AccountPath = "11~11000~11190~11110",
            HierarchyLevel = 4,
            ParentAccountCode = "11190",
            DisplayOrder = 1
        });

        await repository.DeleteAsync("11110");

        var deleted = await repository.FindByCodeAsync("11110");
        deleted.Should().BeNull();
    }
}
