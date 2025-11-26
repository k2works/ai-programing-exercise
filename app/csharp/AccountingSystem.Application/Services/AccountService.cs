using AccountingSystem.Application.Exceptions;
using AccountingSystem.Infrastructure.Entities;
using AccountingSystem.Infrastructure.Repositories;

namespace AccountingSystem.Application.Services;

/// <summary>
/// 勘定科目サービス実装
/// </summary>
public class AccountService : IAccountService
{
    private readonly AccountRepository _accountRepository;

    public AccountService(AccountRepository accountRepository)
    {
        _accountRepository = accountRepository;
    }

    public async Task<IReadOnlyList<Account>> GetAllAccountsAsync()
    {
        var accounts = await _accountRepository.FindAllAsync();
        return accounts.ToList();
    }

    public async Task<Account> GetAccountByCodeAsync(string accountCode)
    {
        var account = await _accountRepository.FindByCodeAsync(accountCode);

        if (account == null)
        {
            throw new AccountNotFoundException($"科目コード {accountCode} が見つかりません");
        }

        return account;
    }

    public async Task<IReadOnlyList<Account>> GetAccountsByBsplTypeAsync(string bsplType)
    {
        if (bsplType != "B" && bsplType != "P")
        {
            throw new ArgumentException("BSPL区分は 'B' または 'P' である必要があります", nameof(bsplType));
        }

        var allAccounts = await _accountRepository.FindAllAsync();
        return allAccounts.Where(a => a.BsplType == bsplType).ToList();
    }

    public async Task<IReadOnlyList<Account>> GetAccountsByTypeAsync(string accountType)
    {
        var accounts = await _accountRepository.FindByTypeAsync(accountType);
        return accounts.ToList();
    }

    public async Task<Account> CreateAccountAsync(Account account)
    {
        ValidateAccount(account);

        var existing = await _accountRepository.FindByCodeAsync(account.AccountCode);
        if (existing != null)
        {
            throw new DuplicateAccountException($"科目コード {account.AccountCode} は既に存在します");
        }

        var accountId = await _accountRepository.InsertAsync(account);
        return (await _accountRepository.FindByIdAsync(accountId))!;
    }

    public async Task<Account> UpdateAccountAsync(string accountCode, Account account)
    {
        var existing = await _accountRepository.FindByCodeAsync(accountCode);
        if (existing == null)
        {
            throw new AccountNotFoundException($"科目コード {accountCode} が見つかりません");
        }

        ValidateAccount(account);

        var updatedAccount = account with { AccountCode = accountCode };
        await _accountRepository.UpdateAsync(updatedAccount);
        return (await _accountRepository.FindByCodeAsync(accountCode))!;
    }

    public async Task DeleteAccountAsync(string accountCode)
    {
        var existing = await _accountRepository.FindByCodeAsync(accountCode);
        if (existing == null)
        {
            throw new AccountNotFoundException($"科目コード {accountCode} が見つかりません");
        }

        await _accountRepository.DeleteAsync(accountCode);
    }

    private static void ValidateAccount(Account account)
    {
        if (string.IsNullOrWhiteSpace(account.AccountCode))
        {
            throw new ArgumentException("勘定科目コードは必須です", nameof(account));
        }

        if (string.IsNullOrWhiteSpace(account.AccountName))
        {
            throw new ArgumentException("勘定科目名は必須です", nameof(account));
        }

        if (string.IsNullOrWhiteSpace(account.AccountType))
        {
            throw new ArgumentException("勘定科目種別は必須です", nameof(account));
        }
    }
}
