namespace ManagementAccounting.Domain.Models

open System

/// <summary>
/// 財務データ（財務会計サービスから取得・変換したデータ）
/// </summary>
type FinancialData = {
    FiscalYear: int
    Sales: decimal
    OperatingProfit: decimal
    TotalAssets: decimal
    TangibleFixedAssets: decimal
    CurrentAssets: decimal
    CurrentLiabilities: decimal
    QuickAssets: decimal
    Equity: decimal
}

/// <summary>
/// 財務比率
/// </summary>
type FinancialRatios = {
    /// 売上高営業利益率
    OperatingProfitMargin: decimal
    /// 総資産回転率
    TotalAssetTurnover: decimal
    /// 固定資産回転率
    FixedAssetTurnover: decimal
    /// 流動比率
    CurrentRatio: decimal
    /// 当座比率
    QuickRatio: decimal
    /// 自己資本比率
    EquityRatio: decimal
    /// ROA（総資産利益率）
    ReturnOnAssets: decimal
    /// ROE（自己資本利益率）
    ReturnOnEquity: decimal
}

/// <summary>
/// 財務分析結果
/// </summary>
type FinancialAnalysisResult = {
    FiscalYear: int
    Data: FinancialData
    Ratios: FinancialRatios
    AnalyzedAt: DateTime
}

/// <summary>
/// 財務分析キャッシュ
/// </summary>
type FinancialAnalysisCache = {
    Id: int option
    FiscalYear: int
    Data: FinancialData
    Ratios: FinancialRatios
    CachedAt: DateTime
}

/// <summary>
/// 財務比率計算モジュール
/// </summary>
module FinancialRatios =

    let calculate (data: FinancialData) : FinancialRatios =
        let safeDivide numerator denominator =
            if denominator = 0m then 0m
            else numerator / denominator

        {
            OperatingProfitMargin = safeDivide data.OperatingProfit data.Sales
            TotalAssetTurnover = safeDivide data.Sales data.TotalAssets
            FixedAssetTurnover = safeDivide data.Sales data.TangibleFixedAssets
            CurrentRatio = safeDivide data.CurrentAssets data.CurrentLiabilities
            QuickRatio = safeDivide data.QuickAssets data.CurrentLiabilities
            EquityRatio = safeDivide data.Equity data.TotalAssets
            ReturnOnAssets = safeDivide data.OperatingProfit data.TotalAssets
            ReturnOnEquity = safeDivide data.OperatingProfit data.Equity
        }
