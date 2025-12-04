namespace AccountingSystem.Infrastructure.DAO

open AccountingSystem.Domain.Models

/// <summary>
/// 貸借対照表の勘定科目残高 DAO（Dapper マッピング用）
/// </summary>
[<CLIMutable>]
type BalanceSheetAccountDao = {
    account_code: string
    account_name: string
    balance: decimal
    element_type: string
}

module BalanceSheetAccountDao =
    /// DAO から BalanceSheetItem に変換
    let toBalanceSheetItem (dao: BalanceSheetAccountDao) : BalanceSheetItem =
        {
            AccountCode = dao.account_code
            AccountName = dao.account_name
            Balance = abs dao.balance
            Percentage = 0M  // 後で計算
        }

/// <summary>
/// 損益計算書の勘定科目残高 DAO（Dapper マッピング用）
/// </summary>
[<CLIMutable>]
type IncomeStatementAccountDao = {
    account_code: string
    account_name: string
    balance: decimal
    element_type: string
    expense_type: string
}

module IncomeStatementAccountDao =
    /// DAO から IncomeStatementItem に変換
    let toIncomeStatementItem (dao: IncomeStatementAccountDao) : IncomeStatementItem =
        {
            AccountCode = dao.account_code
            AccountName = dao.account_name
            Balance = abs dao.balance
            Percentage = 0M  // 後で計算
        }
