namespace ProductionManagement.Domain.Models.Purchase;

/// <summary>
/// 検査結果区分
/// </summary>
public enum InspectionResult
{
    /// <summary>合格</summary>
    Passed,
    /// <summary>不合格</summary>
    Failed,
    /// <summary>条件付合格</summary>
    ConditionallyPassed
}

/// <summary>
/// InspectionResult 拡張メソッド
/// </summary>
public static class InspectionResultExtensions
{
    private static readonly Dictionary<InspectionResult, string> DisplayNames = new()
    {
        { InspectionResult.Passed, "合格" },
        { InspectionResult.Failed, "不合格" },
        { InspectionResult.ConditionallyPassed, "条件付合格" }
    };

    private static readonly Dictionary<string, InspectionResult> FromDisplayNames =
        DisplayNames.ToDictionary(x => x.Value, x => x.Key);

    public static string GetDisplayName(this InspectionResult result) => DisplayNames[result];

    public static InspectionResult FromDisplayName(string displayName)
    {
        if (FromDisplayNames.TryGetValue(displayName, out var result))
        {
            return result;
        }
        throw new ArgumentException($"Unknown inspection result: {displayName}");
    }
}
