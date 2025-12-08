namespace ManagementAccounting.Application.Ports.Out

open System.Threading.Tasks
open ManagementAccounting.Domain.Models

/// <summary>
/// 財務データ取得ポート（財務会計サービスとの連携）
/// </summary>
type IFinancialDataPort =
    /// <summary>
    /// 会計年度を指定して財務データを取得
    /// </summary>
    abstract member FetchFinancialDataByFiscalYearAsync: fiscalYear: int -> Task<FinancialData>

/// <summary>
/// 財務分析キャッシュリポジトリ
/// </summary>
type IFinancialAnalysisCacheRepository =
    /// <summary>
    /// キャッシュを保存
    /// </summary>
    abstract member SaveAsync: cache: FinancialAnalysisCache -> Task<FinancialAnalysisCache>

    /// <summary>
    /// 会計年度でキャッシュを取得
    /// </summary>
    abstract member GetByFiscalYearAsync: fiscalYear: int -> Task<FinancialAnalysisCache option>

    /// <summary>
    /// キャッシュを削除
    /// </summary>
    abstract member DeleteByFiscalYearAsync: fiscalYear: int -> Task<bool>
