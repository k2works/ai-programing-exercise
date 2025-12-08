namespace ManagementAccounting.Application.Ports.In

open System.Threading.Tasks
open ManagementAccounting.Domain.Models

/// <summary>
/// 財務分析リクエスト
/// </summary>
type AnalyzeFinancialDataRequest = {
    FiscalYear: int
    UseCache: bool
}

/// <summary>
/// 財務分析ユースケースインターフェース
/// </summary>
type IFinancialAnalysisUseCase =
    /// <summary>
    /// 財務分析を実行
    /// </summary>
    abstract member AnalyzeAsync: request: AnalyzeFinancialDataRequest -> Task<Result<FinancialAnalysisResult, string>>

    /// <summary>
    /// キャッシュされた分析結果を取得
    /// </summary>
    abstract member GetCachedAnalysisAsync: fiscalYear: int -> Task<FinancialAnalysisResult option>

    /// <summary>
    /// キャッシュを無効化
    /// </summary>
    abstract member InvalidateCacheAsync: fiscalYear: int -> Task<bool>
