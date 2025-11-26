using AccountingSystem.Infrastructure.Entities;

namespace AccountingSystem.Application.Services;

/// <summary>
/// 勘定科目サービスインターフェース
/// </summary>
public interface IAccountService
{
    /// <summary>
    /// すべての勘定科目を取得
    /// </summary>
    Task<IReadOnlyList<Account>> GetAllAccountsAsync();

    /// <summary>
    /// 科目コードで勘定科目を取得
    /// </summary>
    Task<Account> GetAccountByCodeAsync(string accountCode);

    /// <summary>
    /// BSPL 区分で勘定科目を取得
    /// </summary>
    Task<IReadOnlyList<Account>> GetAccountsByBsplTypeAsync(string bsplType);

    /// <summary>
    /// 勘定科目種別で勘定科目を取得
    /// </summary>
    Task<IReadOnlyList<Account>> GetAccountsByTypeAsync(string accountType);

    /// <summary>
    /// 新しい勘定科目を作成
    /// </summary>
    Task<Account> CreateAccountAsync(Account account);

    /// <summary>
    /// 勘定科目を更新
    /// </summary>
    Task<Account> UpdateAccountAsync(string accountCode, Account account);

    /// <summary>
    /// 勘定科目を削除
    /// </summary>
    Task DeleteAccountAsync(string accountCode);
}
