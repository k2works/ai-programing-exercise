namespace AccountingSystem.Application.Port.Out

open System
open System.Threading.Tasks

/// <summary>
/// 日次残高更新パラメータ
/// </summary>
type DailyBalanceUpdateParams = {
    /// 起票日
    EntryDate: DateTime
    /// 勘定科目コード
    AccountCode: string
    /// 補助科目コード
    SubAccountCode: string option
    /// 部門コード
    DepartmentCode: string option
    /// プロジェクトコード
    ProjectCode: string option
    /// 決算仕訳フラグ
    SettlementFlag: int option
    /// 借方金額
    DebitAmount: decimal
    /// 貸方金額
    CreditAmount: decimal
}

/// <summary>
/// 月次残高集計パラメータ
/// </summary>
type MonthlyBalanceConsolidateParams = {
    /// 決算期（会計年度）
    FiscalYear: int
    /// 月度
    Month: int
}

/// <summary>
/// 残高リポジトリインターフェース
/// Infrastructure 層で実装される
/// </summary>
type IBalanceRepository =
    /// <summary>
    /// 日次残高を更新（UPSERT）
    /// 同じキーの残高がある場合は金額を加算する
    /// </summary>
    abstract member UpdateDailyBalanceAsync: DailyBalanceUpdateParams -> Task<unit>

    /// <summary>
    /// 仕訳貸借明細から日次残高を一括更新
    /// 指定された仕訳番号の明細を集計して残高テーブルを更新
    /// </summary>
    abstract member UpdateBalanceFromJournalItemsAsync: journalNo: string -> Task<unit>

    /// <summary>
    /// 指定期間の日次残高を月次残高に集計
    /// </summary>
    abstract member ConsolidateMonthlyBalanceAsync: MonthlyBalanceConsolidateParams -> Task<unit>
