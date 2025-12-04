namespace AccountingSystem.Domain.Models

open System

/// <summary>
/// 損益計算書の項目（勘定科目ごとの金額と構成比率）
/// </summary>
type IncomeStatementItem = {
    /// 勘定科目コード
    AccountCode: string
    /// 勘定科目名
    AccountName: string
    /// 金額
    Balance: decimal
    /// 対売上比率（%）
    Percentage: decimal
}

/// <summary>
/// 損益計算書（Income Statement / P/L）
/// </summary>
type IncomeStatement = {
    /// 開始日
    FromDate: DateTime
    /// 終了日
    ToDate: DateTime
    /// 収益項目
    Revenues: IncomeStatementItem list
    /// 費用項目
    Expenses: IncomeStatementItem list
    /// 売上総利益
    GrossProfit: decimal
    /// 営業利益
    OperatingIncome: decimal
    /// 当期純利益
    NetIncome: decimal
    /// 総収益
    TotalRevenues: decimal
    /// 総費用
    TotalExpenses: decimal
}

module IncomeStatement =
    /// <summary>
    /// 空の損益計算書を作成
    /// </summary>
    let empty fromDate toDate = {
        FromDate = fromDate
        ToDate = toDate
        Revenues = []
        Expenses = []
        GrossProfit = 0M
        OperatingIncome = 0M
        NetIncome = 0M
        TotalRevenues = 0M
        TotalExpenses = 0M
    }

    /// <summary>
    /// 損益が正しく計算されているか検証
    /// 当期純利益 = 総収益 - 総費用
    /// </summary>
    let isValid (incomeStatement: IncomeStatement) =
        incomeStatement.NetIncome = incomeStatement.TotalRevenues - incomeStatement.TotalExpenses
