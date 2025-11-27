using ManagementAccounting.Domain.Entities;

namespace ManagementAccounting.Application.Ports.In;

/// <summary>
/// 財務分析ユースケースインターフェース（入力ポート）
/// </summary>
public interface IAnalyzeFinancialDataUseCase
{
    /// <summary>
    /// 指定された会計年度の財務分析を実行
    /// </summary>
    Task<FinancialAnalysisResult> AnalyzeAsync(int fiscalYear);

    /// <summary>
    /// 複数期間の比較分析を実行
    /// </summary>
    Task<List<FinancialAnalysisResult>> CompareAsync(List<int> fiscalYears);
}
