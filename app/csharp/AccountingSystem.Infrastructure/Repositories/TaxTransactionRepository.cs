using AccountingSystem.Infrastructure.Entities;
using Dapper;
using Npgsql;

namespace AccountingSystem.Infrastructure.Repositories;

/// <summary>
/// 課税取引リポジトリ
/// </summary>
public class TaxTransactionRepository
{
    private readonly string _connectionString;

    public TaxTransactionRepository(string connectionString)
    {
        _connectionString = connectionString;

        // Dapper カラムマッピング設定
        SqlMapper.SetTypeMap(
            typeof(TaxTransaction),
            new CustomPropertyTypeMap(
                typeof(TaxTransaction),
                (type, columnName) => columnName switch
                {
                    "課税取引コード" => type.GetProperty(nameof(TaxTransaction.TaxCode)),
                    "課税取引名" => type.GetProperty(nameof(TaxTransaction.TaxName)),
                    "税率" => type.GetProperty(nameof(TaxTransaction.TaxRate)),
                    "説明" => type.GetProperty(nameof(TaxTransaction.Description)),
                    "有効フラグ" => type.GetProperty(nameof(TaxTransaction.IsActive)),
                    "作成日時" => type.GetProperty(nameof(TaxTransaction.CreatedAt)),
                    "更新日時" => type.GetProperty(nameof(TaxTransaction.UpdatedAt)),
                    _ => null
                }
            )
        );
    }

    public async Task<TaxTransaction?> FindByCodeAsync(string taxCode)
    {
        await using var conn = new NpgsqlConnection(_connectionString);
        await conn.OpenAsync();

        var sql = @"
            SELECT * FROM ""課税取引マスタ""
            WHERE ""課税取引コード"" = @TaxCode
        ";

        return await conn.QuerySingleOrDefaultAsync<TaxTransaction>(sql, new { TaxCode = taxCode });
    }

    public async Task<IEnumerable<TaxTransaction>> FindAllAsync()
    {
        await using var conn = new NpgsqlConnection(_connectionString);
        await conn.OpenAsync();

        var sql = @"
            SELECT * FROM ""課税取引マスタ""
            ORDER BY ""課税取引コード""
        ";

        return await conn.QueryAsync<TaxTransaction>(sql);
    }

    public async Task<IEnumerable<TaxTransaction>> FindActiveAsync()
    {
        await using var conn = new NpgsqlConnection(_connectionString);
        await conn.OpenAsync();

        var sql = @"
            SELECT * FROM ""課税取引マスタ""
            WHERE ""有効フラグ"" = true
            ORDER BY ""課税取引コード""
        ";

        return await conn.QueryAsync<TaxTransaction>(sql);
    }

    public async Task InsertAsync(TaxTransaction taxTransaction)
    {
        await using var conn = new NpgsqlConnection(_connectionString);
        await conn.OpenAsync();

        var sql = @"
            INSERT INTO ""課税取引マスタ"" (
                ""課税取引コード"",
                ""課税取引名"",
                ""税率"",
                ""説明"",
                ""有効フラグ"",
                ""作成日時"",
                ""更新日時""
            ) VALUES (
                @TaxCode,
                @TaxName,
                @TaxRate,
                @Description,
                @IsActive,
                CURRENT_TIMESTAMP,
                CURRENT_TIMESTAMP
            )
        ";

        await conn.ExecuteAsync(sql, taxTransaction);
    }

    public async Task UpdateAsync(TaxTransaction taxTransaction)
    {
        await using var conn = new NpgsqlConnection(_connectionString);
        await conn.OpenAsync();

        var sql = @"
            UPDATE ""課税取引マスタ""
            SET ""課税取引名"" = @TaxName,
                ""税率"" = @TaxRate,
                ""説明"" = @Description,
                ""有効フラグ"" = @IsActive,
                ""更新日時"" = CURRENT_TIMESTAMP
            WHERE ""課税取引コード"" = @TaxCode
        ";

        await conn.ExecuteAsync(sql, taxTransaction);
    }

    public async Task DeleteAsync(string taxCode)
    {
        await using var conn = new NpgsqlConnection(_connectionString);
        await conn.OpenAsync();

        var sql = @"
            DELETE FROM ""課税取引マスタ""
            WHERE ""課税取引コード"" = @TaxCode
        ";

        await conn.ExecuteAsync(sql, new { TaxCode = taxCode });
    }
}
