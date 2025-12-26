namespace ProductionManagement.Domain.Models.Calendar;

/// <summary>
/// カレンダマスタ
/// </summary>
public class WorkCalendar
{
    private string _dateTypeValue = "稼働日";

    public required string CalendarCode { get; set; }

    public DateOnly Date { get; set; }

    /// <summary>
    /// 日付区分（データベース格納用の日本語文字列）
    /// </summary>
    public string DateTypeValue
    {
        get => _dateTypeValue;
        set => _dateTypeValue = value;
    }

    /// <summary>
    /// 日付区分（ドメインロジック用の Enum）
    /// </summary>
    public DateType DateType
    {
        get => DateTypeExtensions.FromDisplayName(_dateTypeValue);
        set => _dateTypeValue = value.GetDisplayName();
    }

    public decimal? WorkingHours { get; set; }

    public string? Note { get; set; }

    public DateTime CreatedAt { get; set; }

    public DateTime UpdatedAt { get; set; }
}
