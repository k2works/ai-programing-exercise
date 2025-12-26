using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Supplier;
using ProductionManagement.Infrastructure.Persistence.TypeHandlers;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 取引先リポジトリ実装
/// </summary>
public class SupplierRepository : ISupplierRepository
{
    private readonly string _connectionString;

    static SupplierRepository()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
        SqlMapper.AddTypeHandler(new SupplierTypeTypeHandler());
    }

    public SupplierRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task SaveAsync(Supplier supplier)
    {
        const string sql = """
            INSERT INTO "取引先マスタ" (
                "取引先コード", "適用開始日", "適用停止日", "取引先名", "取引先カナ",
                "取引先区分", "郵便番号", "住所", "電話番号", "FAX番号", "担当者名"
            ) VALUES (
                @SupplierCode, @EffectiveFrom, @EffectiveTo, @SupplierName, @SupplierNameKana,
                @SupplierType::取引先区分, @PostalCode, @Address, @PhoneNumber, @FaxNumber, @ContactPerson
            )
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql, new
        {
            supplier.SupplierCode,
            supplier.EffectiveFrom,
            supplier.EffectiveTo,
            supplier.SupplierName,
            supplier.SupplierNameKana,
            SupplierType = supplier.SupplierType.GetDisplayName(),
            supplier.PostalCode,
            supplier.Address,
            supplier.PhoneNumber,
            supplier.FaxNumber,
            supplier.ContactPerson
        });
    }

    public async Task<Supplier?> FindByCodeAsync(string supplierCode)
    {
        const string sql = """
            SELECT
                "取引先コード" as SupplierCode,
                "適用開始日" as EffectiveFrom,
                "適用停止日" as EffectiveTo,
                "取引先名" as SupplierName,
                "取引先カナ" as SupplierNameKana,
                "取引先区分"::TEXT as SupplierTypeValue,
                "郵便番号" as PostalCode,
                "住所" as Address,
                "電話番号" as PhoneNumber,
                "FAX番号" as FaxNumber,
                "担当者名" as ContactPerson,
                "作成日時" as CreatedAt,
                "更新日時" as UpdatedAt
            FROM "取引先マスタ"
            WHERE "取引先コード" = @SupplierCode
            ORDER BY "適用開始日" DESC
            LIMIT 1
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QuerySingleOrDefaultAsync<Supplier>(sql, new { SupplierCode = supplierCode });
    }

    public async Task<Supplier?> FindByCodeAndDateAsync(string supplierCode, DateOnly baseDate)
    {
        const string sql = """
            SELECT
                "取引先コード" as SupplierCode,
                "適用開始日" as EffectiveFrom,
                "適用停止日" as EffectiveTo,
                "取引先名" as SupplierName,
                "取引先カナ" as SupplierNameKana,
                "取引先区分"::TEXT as SupplierTypeValue,
                "郵便番号" as PostalCode,
                "住所" as Address,
                "電話番号" as PhoneNumber,
                "FAX番号" as FaxNumber,
                "担当者名" as ContactPerson,
                "作成日時" as CreatedAt,
                "更新日時" as UpdatedAt
            FROM "取引先マスタ"
            WHERE "取引先コード" = @SupplierCode
              AND "適用開始日" <= @BaseDate
              AND ("適用停止日" IS NULL OR "適用停止日" > @BaseDate)
            ORDER BY "適用開始日" DESC
            LIMIT 1
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QuerySingleOrDefaultAsync<Supplier>(sql, new { SupplierCode = supplierCode, BaseDate = baseDate });
    }

    public async Task<IReadOnlyList<Supplier>> FindByTypeAsync(SupplierType supplierType)
    {
        const string sql = """
            SELECT
                "取引先コード" as SupplierCode,
                "適用開始日" as EffectiveFrom,
                "適用停止日" as EffectiveTo,
                "取引先名" as SupplierName,
                "取引先カナ" as SupplierNameKana,
                "取引先区分"::TEXT as SupplierTypeValue,
                "郵便番号" as PostalCode,
                "住所" as Address,
                "電話番号" as PhoneNumber,
                "FAX番号" as FaxNumber,
                "担当者名" as ContactPerson,
                "作成日時" as CreatedAt,
                "更新日時" as UpdatedAt
            FROM "取引先マスタ"
            WHERE "取引先区分" = @SupplierType::取引先区分
              AND ("適用停止日" IS NULL OR "適用停止日" > CURRENT_DATE)
            ORDER BY "取引先コード", "適用開始日" DESC
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        var result = await connection.QueryAsync<Supplier>(sql, new { SupplierType = supplierType.GetDisplayName() });
        return result.ToList();
    }

    public async Task<IReadOnlyList<Supplier>> FindAllAsync()
    {
        const string sql = """
            SELECT
                "取引先コード" as SupplierCode,
                "適用開始日" as EffectiveFrom,
                "適用停止日" as EffectiveTo,
                "取引先名" as SupplierName,
                "取引先カナ" as SupplierNameKana,
                "取引先区分"::TEXT as SupplierTypeValue,
                "郵便番号" as PostalCode,
                "住所" as Address,
                "電話番号" as PhoneNumber,
                "FAX番号" as FaxNumber,
                "担当者名" as ContactPerson,
                "作成日時" as CreatedAt,
                "更新日時" as UpdatedAt
            FROM "取引先マスタ"
            ORDER BY "取引先コード", "適用開始日" DESC
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        var result = await connection.QueryAsync<Supplier>(sql);
        return result.ToList();
    }

    public async Task DeleteAllAsync()
    {
        const string sql = """TRUNCATE TABLE "取引先マスタ" CASCADE""";

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql);
    }
}
