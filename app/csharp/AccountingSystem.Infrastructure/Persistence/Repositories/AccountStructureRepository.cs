using AccountingSystem.Domain.Entities;
using Dapper;
using Npgsql;

namespace AccountingSystem.Infrastructure.Persistence.Repositories;

/// <summary>
/// 勘定科目構成リポジトリ
/// </summary>
public class AccountStructureRepository
{
    private readonly string _connectionString;

    public AccountStructureRepository(string connectionString)
    {
        _connectionString = connectionString;

        // Dapper カラムマッピング設定（日本語カラム名 → 英語プロパティ名）
        SqlMapper.SetTypeMap(
            typeof(AccountStructure),
            new CustomPropertyTypeMap(
                typeof(AccountStructure),
                (type, columnName) => columnName switch
                {
                    "勘定科目コード" => type.GetProperty(nameof(AccountStructure.AccountCode)),
                    "勘定科目パス" => type.GetProperty(nameof(AccountStructure.AccountPath)),
                    "階層レベル" => type.GetProperty(nameof(AccountStructure.HierarchyLevel)),
                    "親科目コード" => type.GetProperty(nameof(AccountStructure.ParentAccountCode)),
                    "表示順序" => type.GetProperty(nameof(AccountStructure.DisplayOrder)),
                    "作成日時" => type.GetProperty(nameof(AccountStructure.CreatedAt)),
                    "更新日時" => type.GetProperty(nameof(AccountStructure.UpdatedAt)),
                    _ => null
                }
            )
        );
    }

    public async Task InsertAsync(AccountStructure accountStructure)
    {
        await using var conn = new NpgsqlConnection(_connectionString);
        await conn.OpenAsync();

        var sql = @"
            INSERT INTO ""勘定科目構成マスタ"" (
                ""勘定科目コード"",
                ""勘定科目パス"",
                ""階層レベル"",
                ""親科目コード"",
                ""表示順序"",
                ""作成日時"",
                ""更新日時""
            ) VALUES (
                @AccountCode,
                @AccountPath,
                @HierarchyLevel,
                @ParentAccountCode,
                @DisplayOrder,
                CURRENT_TIMESTAMP,
                CURRENT_TIMESTAMP
            )
        ";

        await conn.ExecuteAsync(sql, accountStructure);
    }

    public async Task<AccountStructure?> FindByCodeAsync(string accountCode)
    {
        await using var conn = new NpgsqlConnection(_connectionString);
        await conn.OpenAsync();

        var sql = @"
            SELECT * FROM ""勘定科目構成マスタ""
            WHERE ""勘定科目コード"" = @AccountCode
        ";

        return await conn.QuerySingleOrDefaultAsync<AccountStructure>(sql, new { AccountCode = accountCode });
    }

    public async Task<IEnumerable<AccountStructure>> FindAllAsync()
    {
        await using var conn = new NpgsqlConnection(_connectionString);
        await conn.OpenAsync();

        var sql = @"
            SELECT * FROM ""勘定科目構成マスタ""
            ORDER BY ""勘定科目パス""
        ";

        return await conn.QueryAsync<AccountStructure>(sql);
    }

    public async Task<IEnumerable<AccountStructure>> FindChildrenAsync(string accountCode)
    {
        await using var conn = new NpgsqlConnection(_connectionString);
        await conn.OpenAsync();

        var sql = @"
            SELECT * FROM ""勘定科目構成マスタ""
            WHERE ""勘定科目パス"" LIKE CONCAT('%~', @AccountCode, '~%')
               OR ""勘定科目コード"" = @AccountCode
            ORDER BY ""勘定科目パス""
        ";

        return await conn.QueryAsync<AccountStructure>(sql, new { AccountCode = accountCode });
    }

    public async Task<IEnumerable<AccountStructure>> FindByLevelAsync(int hierarchyLevel)
    {
        await using var conn = new NpgsqlConnection(_connectionString);
        await conn.OpenAsync();

        var sql = @"
            SELECT * FROM ""勘定科目構成マスタ""
            WHERE ""階層レベル"" = @HierarchyLevel
            ORDER BY ""表示順序"", ""勘定科目コード""
        ";

        return await conn.QueryAsync<AccountStructure>(sql, new { HierarchyLevel = hierarchyLevel });
    }

    public async Task UpdateAsync(AccountStructure accountStructure)
    {
        await using var conn = new NpgsqlConnection(_connectionString);
        await conn.OpenAsync();

        var sql = @"
            UPDATE ""勘定科目構成マスタ""
            SET ""勘定科目パス"" = @AccountPath,
                ""階層レベル"" = @HierarchyLevel,
                ""親科目コード"" = @ParentAccountCode,
                ""表示順序"" = @DisplayOrder,
                ""更新日時"" = CURRENT_TIMESTAMP
            WHERE ""勘定科目コード"" = @AccountCode
        ";

        await conn.ExecuteAsync(sql, accountStructure);
    }

    public async Task DeleteAsync(string accountCode)
    {
        await using var conn = new NpgsqlConnection(_connectionString);
        await conn.OpenAsync();

        var sql = @"
            DELETE FROM ""勘定科目構成マスタ""
            WHERE ""勘定科目コード"" = @AccountCode
        ";

        await conn.ExecuteAsync(sql, new { AccountCode = accountCode });
    }
}
