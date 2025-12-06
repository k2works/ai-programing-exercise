namespace AccountingSystem.Application.Port.In

open System.Threading.Tasks
open AccountingSystem.Domain.Models

/// <summary>
/// 財務分析ユースケースインターフェース（Input Port）
/// D社の事例に基づく包括的な財務分析機能を提供
/// </summary>
type IFinancialAnalysisUseCase =
    /// <summary>
    /// 指定した会計年度の財務データを取得する
    /// </summary>
    /// <param name="fiscalYear">会計年度（例: 2021）</param>
    /// <returns>財務データ</returns>
    abstract member GetFinancialDataAsync: fiscalYear:int -> Task<Result<FinancialData, string>>

    /// <summary>
    /// 指定した会計年度の包括的な財務指標を計算する
    /// </summary>
    /// <param name="fiscalYear">会計年度（例: 2021）</param>
    /// <returns>包括的な財務指標</returns>
    abstract member CalculateComprehensiveRatiosAsync: fiscalYear:int -> Task<Result<ComprehensiveFinancialRatios, string>>

    /// <summary>
    /// 複数年度の財務指標を比較する
    /// </summary>
    /// <param name="fiscalYears">会計年度のリスト</param>
    /// <returns>年度ごとの財務指標リスト</returns>
    abstract member CompareFinancialRatiosAsync: fiscalYears:int list -> Task<Result<ComprehensiveFinancialRatios list, string>>

    /// <summary>
    /// 収益性分析を実行する
    /// </summary>
    /// <param name="fiscalYear">会計年度</param>
    /// <returns>収益性指標のサマリー</returns>
    abstract member AnalyzeProfitabilityAsync: fiscalYear:int -> Task<Result<ProfitabilityAnalysis, string>>

    /// <summary>
    /// 効率性分析を実行する
    /// </summary>
    /// <param name="fiscalYear">会計年度</param>
    /// <returns>効率性指標のサマリー</returns>
    abstract member AnalyzeEfficiencyAsync: fiscalYear:int -> Task<Result<EfficiencyAnalysis, string>>

    /// <summary>
    /// 安全性分析を実行する
    /// </summary>
    /// <param name="fiscalYear">会計年度</param>
    /// <returns>安全性指標のサマリー</returns>
    abstract member AnalyzeSafetyAsync: fiscalYear:int -> Task<Result<SafetyAnalysis, string>>

/// <summary>
/// 収益性分析結果
/// </summary>
and ProfitabilityAnalysis = {
    FiscalYear: int
    GrossProfitMargin: decimal
    OperatingProfitMargin: decimal
    OrdinaryProfitMargin: decimal
    SellingExpenseRatio: decimal
    Roa: decimal
    Roe: decimal
    Analysis: string
}

/// <summary>
/// 効率性分析結果
/// </summary>
and EfficiencyAnalysis = {
    FiscalYear: int
    TotalAssetTurnover: decimal
    AccountsReceivableTurnover: decimal
    InventoryTurnover: decimal
    TangibleFixedAssetTurnover: decimal
    Analysis: string
}

/// <summary>
/// 安全性分析結果
/// </summary>
and SafetyAnalysis = {
    FiscalYear: int
    CurrentRatio: decimal
    QuickRatio: decimal
    FixedRatio: decimal
    FixedLongTermSuitabilityRatio: decimal
    DebtRatio: decimal
    EquityRatio: decimal
    Analysis: string
}
