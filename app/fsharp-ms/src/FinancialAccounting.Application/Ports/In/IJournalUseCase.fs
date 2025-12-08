namespace FinancialAccounting.Application.Ports.In

open System.Threading.Tasks
open FinancialAccounting.Domain.Entities

/// <summary>
/// 仕訳作成リクエスト
/// </summary>
type CreateJournalRequest = {
    JournalDate: System.DateTime
    Description: string
    FiscalYear: int
    Entries: CreateJournalEntryRequest list
}

and CreateJournalEntryRequest = {
    AccountCode: string
    DebitAmount: decimal
    CreditAmount: decimal
    Description: string
}

/// <summary>
/// 仕訳ユースケースの入力ポート
/// </summary>
type IJournalUseCase =
    /// <summary>
    /// 仕訳を作成
    /// </summary>
    abstract member CreateJournalAsync: CreateJournalRequest -> Task<Result<Journal, string>>

    /// <summary>
    /// IDで仕訳を取得
    /// </summary>
    abstract member GetJournalByIdAsync: int -> Task<Journal option>

    /// <summary>
    /// 会計年度で仕訳一覧を取得
    /// </summary>
    abstract member GetJournalsByFiscalYearAsync: int -> Task<Journal list>
