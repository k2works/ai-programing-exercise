using Dapper;
using Npgsql;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Calendar;
using ProductionManagement.Infrastructure.Persistence.TypeHandlers;

namespace ProductionManagement.Infrastructure.Persistence.Repositories;

/// <summary>
/// カレンダリポジトリ実装
/// </summary>
public class CalendarRepository : ICalendarRepository
{
    private readonly string _connectionString;

    static CalendarRepository()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
        SqlMapper.AddTypeHandler(new DateTypeTypeHandler());
    }

    public CalendarRepository(string connectionString)
    {
        _connectionString = connectionString;
    }

    public async Task SaveAsync(WorkCalendar calendar)
    {
        const string sql = """
            INSERT INTO "カレンダマスタ" (
                "カレンダコード", "日付", "日付区分", "稼働時間", "備考"
            ) VALUES (
                @CalendarCode, @Date, @DateType::日付区分, @WorkingHours, @Note
            )
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql, new
        {
            calendar.CalendarCode,
            calendar.Date,
            DateType = calendar.DateType.GetDisplayName(),
            calendar.WorkingHours,
            calendar.Note
        });
    }

    public async Task<WorkCalendar?> FindByCodeAndDateAsync(string calendarCode, DateOnly date)
    {
        const string sql = """
            SELECT
                "カレンダコード" as CalendarCode,
                "日付" as Date,
                "日付区分"::TEXT as DateTypeValue,
                "稼働時間" as WorkingHours,
                "備考" as Note,
                "作成日時" as CreatedAt,
                "更新日時" as UpdatedAt
            FROM "カレンダマスタ"
            WHERE "カレンダコード" = @CalendarCode
              AND "日付" = @Date
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        return await connection.QuerySingleOrDefaultAsync<WorkCalendar>(sql, new { CalendarCode = calendarCode, Date = date });
    }

    public async Task<IReadOnlyList<WorkCalendar>> FindByCodeAndDateRangeAsync(string calendarCode, DateOnly fromDate, DateOnly toDate)
    {
        const string sql = """
            SELECT
                "カレンダコード" as CalendarCode,
                "日付" as Date,
                "日付区分"::TEXT as DateTypeValue,
                "稼働時間" as WorkingHours,
                "備考" as Note,
                "作成日時" as CreatedAt,
                "更新日時" as UpdatedAt
            FROM "カレンダマスタ"
            WHERE "カレンダコード" = @CalendarCode
              AND "日付" >= @FromDate
              AND "日付" <= @ToDate
            ORDER BY "日付"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        var result = await connection.QueryAsync<WorkCalendar>(sql, new { CalendarCode = calendarCode, FromDate = fromDate, ToDate = toDate });
        return result.ToList();
    }

    public async Task<IReadOnlyList<WorkCalendar>> FindWorkingDaysAsync(string calendarCode, DateOnly fromDate, DateOnly toDate)
    {
        const string sql = """
            SELECT
                "カレンダコード" as CalendarCode,
                "日付" as Date,
                "日付区分"::TEXT as DateTypeValue,
                "稼働時間" as WorkingHours,
                "備考" as Note,
                "作成日時" as CreatedAt,
                "更新日時" as UpdatedAt
            FROM "カレンダマスタ"
            WHERE "カレンダコード" = @CalendarCode
              AND "日付" >= @FromDate
              AND "日付" <= @ToDate
              AND "日付区分" IN ('稼働日', '半日稼働')
            ORDER BY "日付"
            """;

        await using var connection = new NpgsqlConnection(_connectionString);
        var result = await connection.QueryAsync<WorkCalendar>(sql, new { CalendarCode = calendarCode, FromDate = fromDate, ToDate = toDate });
        return result.ToList();
    }

    public async Task DeleteAllAsync()
    {
        const string sql = """TRUNCATE TABLE "カレンダマスタ" CASCADE""";

        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.ExecuteAsync(sql);
    }
}
