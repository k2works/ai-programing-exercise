using ProductionManagement.Domain.Models.Calendar;

namespace ProductionManagement.Application.Port.Out;

/// <summary>
/// カレンダリポジトリ（Output Port）
/// </summary>
public interface ICalendarRepository
{
    /// <summary>
    /// カレンダを保存する
    /// </summary>
    Task SaveAsync(WorkCalendar calendar);

    /// <summary>
    /// カレンダコードと日付でカレンダを検索する
    /// </summary>
    Task<WorkCalendar?> FindByCodeAndDateAsync(string calendarCode, DateOnly date);

    /// <summary>
    /// カレンダコードで期間内のカレンダを検索する
    /// </summary>
    Task<IReadOnlyList<WorkCalendar>> FindByCodeAndDateRangeAsync(string calendarCode, DateOnly fromDate, DateOnly toDate);

    /// <summary>
    /// カレンダコードで稼働日を検索する
    /// </summary>
    Task<IReadOnlyList<WorkCalendar>> FindWorkingDaysAsync(string calendarCode, DateOnly fromDate, DateOnly toDate);

    /// <summary>
    /// すべてのカレンダを削除する
    /// </summary>
    Task DeleteAllAsync();
}
