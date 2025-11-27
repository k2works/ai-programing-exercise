namespace ManagementAccounting.Domain.Entities;

/// <summary>
/// 財務データ（管理会計コンテキストのドメインモデル）
///
/// 財務会計サービスから取得したデータを変換した形式
/// </summary>
public record FinancialData(
    int FiscalYear,
    decimal Sales,
    decimal OperatingProfit,
    decimal TotalAssets,
    decimal TangibleFixedAssets,
    decimal CurrentAssets,
    decimal CurrentLiabilities,
    decimal QuickAssets,
    decimal Equity
);
