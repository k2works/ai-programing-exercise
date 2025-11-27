using AccountingSystem.Domain.Models.Financial;

namespace AccountingSystem.Application.Ports.In;

/// <summary>
/// 財務分析サービスインターフェース（入力ポート）
/// D社の財務データに基づく各種財務指標の分析を提供
/// </summary>
public interface IFinancialAnalysisService
{
    /// <summary>
    /// 指定した決算期の財務分析を実行
    /// </summary>
    /// <param name="fiscalYear">決算期（例: 2021, 2022）</param>
    /// <returns>財務分析結果（収益性・効率性・安全性指標）</returns>
    Task<AnalysisResult> AnalyzeAsync(int fiscalYear);

    /// <summary>
    /// 複数期間の財務分析を実行し、比較可能なデータを返す
    /// </summary>
    /// <param name="fromFiscalYear">開始決算期</param>
    /// <param name="toFiscalYear">終了決算期</param>
    /// <returns>期間ごとの財務分析結果のリスト</returns>
    Task<IReadOnlyList<AnalysisResult>> AnalyzeRangeAsync(int fromFiscalYear, int toFiscalYear);
}
