namespace AccountingSystem.Domain.Models

/// <summary>
/// 勘定科目エンティティ
/// データベースの日本語カラム名と英語プロパティ名をマッピング
/// </summary>
[<CLIMutable>]
type Account = {
    AccountId: int option              // 勘定科目ID
    AccountCode: string                // 勘定科目コード
    AccountName: string                // 勘定科目名
    AccountNameKana: string option     // 勘定科目カナ
    AccountType: string                // 勘定科目種別
    IsSummaryAccount: bool             // 合計科目
    BsplType: string option            // BSPL区分
    TransactionElementType: string option  // 取引要素区分
    ExpenseType: string option         // 費用区分
    DisplayOrder: int                  // 表示順序
    IsAggregationTarget: bool          // 集計対象
    TaxCode: string option             // 課税取引コード
    Balance: decimal                   // 残高
}

/// Account エンティティのファクトリ関数
module Account =
    let create accountCode accountName accountType isSummaryAccount =
        {
            AccountId = None
            AccountCode = accountCode
            AccountName = accountName
            AccountNameKana = None
            AccountType = accountType
            IsSummaryAccount = isSummaryAccount
            BsplType = None
            TransactionElementType = None
            ExpenseType = None
            DisplayOrder = 0
            IsAggregationTarget = true
            TaxCode = None
            Balance = 0m
        }
