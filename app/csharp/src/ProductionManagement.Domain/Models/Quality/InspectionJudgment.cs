namespace ProductionManagement.Domain.Models.Quality;

/// <summary>
/// 検査判定
/// </summary>
public enum InspectionJudgment
{
    Passed,
    Failed,
    Hold
}

/// <summary>
/// 検査判定の拡張メソッド
/// </summary>
public static class InspectionJudgmentExtensions
{
    private static readonly Dictionary<InspectionJudgment, string> DisplayNames = new()
    {
        { InspectionJudgment.Passed, "合格" },
        { InspectionJudgment.Failed, "不合格" },
        { InspectionJudgment.Hold, "保留" }
    };

    public static string ToDisplayName(this InspectionJudgment judgment)
        => DisplayNames[judgment];

    public static InspectionJudgment FromDisplayName(string displayName)
        => DisplayNames.First(x => x.Value == displayName).Key;
}
