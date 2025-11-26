using AccountingSystem.Infrastructure.Entities;

namespace AccountingSystem.Application.Services;

/// <summary>
/// 仕訳サービスインターフェース
/// </summary>
public interface IJournalService
{
    /// <summary>
    /// 仕訳を伝票番号で取得
    /// </summary>
    Task<Journal> GetJournalByNoAsync(string journalNo);

    /// <summary>
    /// 仕訳を作成
    /// </summary>
    Task<Journal> CreateJournalAsync(Journal journal);

    /// <summary>
    /// 仕訳を削除
    /// </summary>
    Task DeleteJournalAsync(string journalNo);

    /// <summary>
    /// 借方・貸方の合計を検証
    /// </summary>
    Task<(decimal DebitTotal, decimal CreditTotal, bool IsBalanced)> ValidateBalanceAsync(string journalNo);
}
