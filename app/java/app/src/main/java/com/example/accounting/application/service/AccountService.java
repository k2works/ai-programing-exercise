package com.example.accounting.application.service;

import com.example.accounting.application.exception.AccountNotFoundException;
import com.example.accounting.application.port.in.AccountUseCase;
import com.example.accounting.application.port.out.AccountRepository;
import com.example.accounting.domain.model.Account;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 勘定科目サービス（Application Service）
 * Input Port の実装としてビジネスロジックを提供
 */
@Service
@Transactional
public class AccountService implements AccountUseCase {

    private final AccountRepository accountRepository;  // Output Portに依存

    public AccountService(AccountRepository accountRepository) {
        this.accountRepository = accountRepository;
    }

    @Override
    @Transactional(readOnly = true)
    public List<Account> getAllAccounts() {
        return accountRepository.findAll();
    }

    @Override
    @Transactional(readOnly = true)
    public Account getAccountByCode(String accountCode) {
        return accountRepository.findByCode(accountCode)
                .orElseThrow(() -> new AccountNotFoundException(
                        "科目コード " + accountCode + " が見つかりません"));
    }

    @Override
    @Transactional(readOnly = true)
    public List<Account> getAccountsByBsplType(String bsplType) {
        if (!bsplType.equals("B") && !bsplType.equals("P")) {
            throw new IllegalArgumentException("BSPL区分は 'B' または 'P' である必要があります");
        }
        return accountRepository.findByBsplType(bsplType);
    }

    @Override
    public Account createAccount(Account account) {
        // ビジネスルール：科目コードの重複チェック
        accountRepository.findByCode(account.getAccountCode()).ifPresent(existing -> {
            throw new IllegalStateException(
                    "科目コード " + account.getAccountCode() + " は既に存在します");
        });

        return accountRepository.save(account);
    }

    @Override
    public Account updateAccount(String accountCode, Account account) {
        // 存在チェック
        getAccountByCode(accountCode);

        // 科目コード変更チェック
        if (!accountCode.equals(account.getAccountCode())) {
            throw new IllegalArgumentException("科目コードは変更できません");
        }

        return accountRepository.save(account);
    }

    @Override
    public void deleteAccount(String accountCode) {
        // 存在チェック
        getAccountByCode(accountCode);

        accountRepository.deleteByCode(accountCode);
    }
}
