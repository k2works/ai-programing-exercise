using AccountingSystem.Domain.Entities;

namespace AccountingSystem.Application.Ports.Out;

/// <summary>
/// 仕訳リポジトリインターフェース（出力ポート）
/// </summary>
public interface IJournalRepository
{
    /// <summary>
    /// 仕訳を登録（ヘッダー + 明細 + 貸借明細）
    /// </summary>
    Task InsertAsync(Journal journal);

    /// <summary>
    /// 仕訳を取得（明細・貸借明細を含む）
    /// </summary>
    Task<Journal?> FindByJournalNoAsync(string journalNo);

    /// <summary>
    /// 仕訳を削除（CASCADE で明細も削除される）
    /// </summary>
    Task DeleteByJournalNoAsync(string journalNo);

    /// <summary>
    /// 借方・貸方の合計を取得（複式簿記の検証用）
    /// </summary>
    Task<(decimal DebitTotal, decimal CreditTotal)> GetBalanceAsync(string journalNo);

    /// <summary>
    /// 決算期（年度）で仕訳一覧を取得
    /// </summary>
    Task<IReadOnlyList<Journal>> FindByFiscalYearAsync(int fiscalYear);
}
