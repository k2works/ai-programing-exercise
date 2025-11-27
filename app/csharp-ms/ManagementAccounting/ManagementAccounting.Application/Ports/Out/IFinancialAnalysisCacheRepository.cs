using ManagementAccounting.Domain.Entities;

namespace ManagementAccounting.Application.Ports.Out;

/// <summary>
/// 財務分析キャッシュリポジトリインターフェース（出力ポート）
/// </summary>
public interface IFinancialAnalysisCacheRepository
{
    /// <summary>
    /// 分析結果をキャッシュに保存
    /// </summary>
    Task<FinancialAnalysisCache> SaveAsync(FinancialAnalysisCache cache);

    /// <summary>
    /// 会計年度でキャッシュを検索
    /// </summary>
    Task<FinancialAnalysisCache?> FindByFiscalYearAsync(int fiscalYear);

    /// <summary>
    /// 全てのキャッシュを取得
    /// </summary>
    Task<List<FinancialAnalysisCache>> FindAllAsync();
}
