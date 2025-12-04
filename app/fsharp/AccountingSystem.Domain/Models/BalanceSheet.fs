namespace AccountingSystem.Domain.Models

open System

/// <summary>
/// 貸借対照表の項目
/// </summary>
type BalanceSheetItem = {
    /// 勘定科目コード
    AccountCode: string
    /// 勘定科目名
    AccountName: string
    /// 残高
    Balance: decimal
    /// 構成比率（%）
    Percentage: decimal
}

/// <summary>
/// 貸借対照表
/// </summary>
type BalanceSheet = {
    /// 基準日
    AsOfDate: DateTime
    /// 資産項目
    Assets: BalanceSheetItem list
    /// 負債項目
    Liabilities: BalanceSheetItem list
    /// 純資産項目
    Equity: BalanceSheetItem list
    /// 資産合計
    TotalAssets: decimal
    /// 負債合計
    TotalLiabilities: decimal
    /// 純資産合計
    TotalEquity: decimal
    /// 負債・純資産合計
    TotalLiabilitiesAndEquity: decimal
}

module BalanceSheet =
    /// <summary>
    /// 空の貸借対照表を作成
    /// </summary>
    let empty asOfDate = {
        AsOfDate = asOfDate
        Assets = []
        Liabilities = []
        Equity = []
        TotalAssets = 0M
        TotalLiabilities = 0M
        TotalEquity = 0M
        TotalLiabilitiesAndEquity = 0M
    }

    /// <summary>
    /// 貸借平均の原則が成立しているか検証
    /// 資産 = 負債 + 純資産
    /// </summary>
    let isBalanced (balanceSheet: BalanceSheet) =
        balanceSheet.TotalAssets = balanceSheet.TotalLiabilitiesAndEquity
