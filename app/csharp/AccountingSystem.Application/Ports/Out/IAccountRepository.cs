using AccountingSystem.Domain.Entities;

namespace AccountingSystem.Application.Ports.Out;

/// <summary>
/// 勘定科目リポジトリインターフェース（出力ポート）
/// </summary>
public interface IAccountRepository
{
    /// <summary>
    /// 勘定科目を登録
    /// </summary>
    Task<int> InsertAsync(Account account);

    /// <summary>
    /// 科目コードで勘定科目を取得
    /// </summary>
    Task<Account?> FindByCodeAsync(string accountCode);

    /// <summary>
    /// 勘定科目IDで勘定科目を取得
    /// </summary>
    Task<Account?> FindByIdAsync(int accountId);

    /// <summary>
    /// すべての勘定科目を取得
    /// </summary>
    Task<IEnumerable<Account>> FindAllAsync();

    /// <summary>
    /// 勘定科目種別で勘定科目を取得
    /// </summary>
    Task<IEnumerable<Account>> FindByTypeAsync(string accountType);

    /// <summary>
    /// 集計科目を取得
    /// </summary>
    Task<IEnumerable<Account>> FindSummaryAccountsAsync();

    /// <summary>
    /// 明細科目を取得
    /// </summary>
    Task<IEnumerable<Account>> FindDetailAccountsAsync();

    /// <summary>
    /// 勘定科目を更新
    /// </summary>
    Task<int> UpdateAsync(Account account);

    /// <summary>
    /// 残高を更新
    /// </summary>
    Task<int> UpdateBalanceAsync(string accountCode, decimal balance);

    /// <summary>
    /// 勘定科目を削除
    /// </summary>
    Task<int> DeleteAsync(string accountCode);
}
