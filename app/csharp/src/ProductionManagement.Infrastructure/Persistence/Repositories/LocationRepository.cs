using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Location;
using ProductionManagement.Infrastructure.Persistence.TypeHandlers;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// 場所リポジトリ実装
/// </summary>
public class LocationRepository : ILocationRepository
{
    private readonly string _connectionString;

    static LocationRepository()
    {
        SqlMapper.AddTypeHandler(new LocationTypeTypeHandler());
    }

    public LocationRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task SaveAsync(Location location)
    {
        const string sql = """
            INSERT INTO "場所マスタ" (
                "場所コード", "場所名", "場所区分", "親場所コード"
            ) VALUES (
                @LocationCode, @LocationName, @LocationType::場所区分, @ParentLocationCode
            )
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql, new
        {
            location.LocationCode,
            location.LocationName,
            LocationType = location.LocationType.GetDisplayName(),
            location.ParentLocationCode
        });
    }

    public async Task<Location?> FindByCodeAsync(string locationCode)
    {
        const string sql = """
            SELECT
                "場所コード" as LocationCode,
                "場所名" as LocationName,
                "場所区分"::TEXT as LocationTypeValue,
                "親場所コード" as ParentLocationCode,
                "作成日時" as CreatedAt,
                "更新日時" as UpdatedAt
            FROM "場所マスタ"
            WHERE "場所コード" = @LocationCode
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QuerySingleOrDefaultAsync<Location>(sql, new { LocationCode = locationCode });
    }

    public async Task<IReadOnlyList<Location>> FindByTypeAsync(LocationType locationType)
    {
        const string sql = """
            SELECT
                "場所コード" as LocationCode,
                "場所名" as LocationName,
                "場所区分"::TEXT as LocationTypeValue,
                "親場所コード" as ParentLocationCode,
                "作成日時" as CreatedAt,
                "更新日時" as UpdatedAt
            FROM "場所マスタ"
            WHERE "場所区分" = @LocationType::場所区分
            ORDER BY "場所コード"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        var result = await connection.QueryAsync<Location>(sql, new { LocationType = locationType.GetDisplayName() });
        return result.ToList();
    }

    public async Task<IReadOnlyList<Location>> FindChildrenAsync(string parentLocationCode)
    {
        const string sql = """
            SELECT
                "場所コード" as LocationCode,
                "場所名" as LocationName,
                "場所区分"::TEXT as LocationTypeValue,
                "親場所コード" as ParentLocationCode,
                "作成日時" as CreatedAt,
                "更新日時" as UpdatedAt
            FROM "場所マスタ"
            WHERE "親場所コード" = @ParentLocationCode
            ORDER BY "場所コード"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        var result = await connection.QueryAsync<Location>(sql, new { ParentLocationCode = parentLocationCode });
        return result.ToList();
    }

    public async Task<IReadOnlyList<Location>> FindAllAsync()
    {
        const string sql = """
            SELECT
                "場所コード" as LocationCode,
                "場所名" as LocationName,
                "場所区分"::TEXT as LocationTypeValue,
                "親場所コード" as ParentLocationCode,
                "作成日時" as CreatedAt,
                "更新日時" as UpdatedAt
            FROM "場所マスタ"
            ORDER BY "場所コード"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        var result = await connection.QueryAsync<Location>(sql);
        return result.ToList();
    }

    public async Task DeleteAllAsync()
    {
        const string sql = """TRUNCATE TABLE "場所マスタ" CASCADE""";

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql);
    }
}
