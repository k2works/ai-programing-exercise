using AccountingSystem.Application.Exceptions;
using AccountingSystem.Domain.Entities;
using AccountingSystem.Application.Ports.In;
using AccountingSystem.Application.Ports.Out;
using AccountingSystem.Domain.Events;
using MediatR;

namespace AccountingSystem.Application.Services;

/// <summary>
/// 勘定科目サービス実装
/// </summary>
public class AccountService : IAccountService
{
    private readonly IAccountRepository _accountRepository;
    private readonly IMediator _mediator;

    public AccountService(IAccountRepository accountRepository, IMediator mediator)
    {
        _accountRepository = accountRepository;
        _mediator = mediator;
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
        var createdAccount = (await _accountRepository.FindByIdAsync(accountId))!;

        // 監査ログイベント発行
        await _mediator.Publish(new AccountCreatedEvent
        {
            AccountCode = createdAccount.AccountCode,
            AccountData = new Dictionary<string, object>
            {
                ["accountCode"] = createdAccount.AccountCode,
                ["accountName"] = createdAccount.AccountName,
                ["accountType"] = createdAccount.AccountType,
                ["bsplType"] = createdAccount.BsplType ?? string.Empty,
                ["taxCode"] = createdAccount.TaxCode ?? string.Empty
            },
            UserId = "system",
            UserName = "システム"
        });

        return createdAccount;
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
        var result = (await _accountRepository.FindByCodeAsync(accountCode))!;

        // 監査ログイベント発行
        await _mediator.Publish(new AccountUpdatedEvent
        {
            AccountCode = accountCode,
            OldValues = new Dictionary<string, object>
            {
                ["accountName"] = existing.AccountName,
                ["accountType"] = existing.AccountType,
                ["bsplType"] = existing.BsplType ?? string.Empty,
                ["taxCode"] = existing.TaxCode ?? string.Empty
            },
            NewValues = new Dictionary<string, object>
            {
                ["accountName"] = result.AccountName,
                ["accountType"] = result.AccountType,
                ["bsplType"] = result.BsplType ?? string.Empty,
                ["taxCode"] = result.TaxCode ?? string.Empty
            },
            UserId = "system",
            UserName = "システム"
        });

        return result;
    }

    public async Task DeleteAccountAsync(string accountCode)
    {
        var existing = await _accountRepository.FindByCodeAsync(accountCode);
        if (existing == null)
        {
            throw new AccountNotFoundException($"科目コード {accountCode} が見つかりません");
        }

        await _accountRepository.DeleteAsync(accountCode);

        // 監査ログイベント発行
        await _mediator.Publish(new AccountDeletedEvent
        {
            AccountCode = accountCode,
            DeletedData = new Dictionary<string, object>
            {
                ["accountCode"] = existing.AccountCode,
                ["accountName"] = existing.AccountName,
                ["accountType"] = existing.AccountType,
                ["bsplType"] = existing.BsplType ?? string.Empty,
                ["taxCode"] = existing.TaxCode ?? string.Empty
            },
            UserId = "system",
            UserName = "システム",
            Reason = "削除操作"
        });
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
