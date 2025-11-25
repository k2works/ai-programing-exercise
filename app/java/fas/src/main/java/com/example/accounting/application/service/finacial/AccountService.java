package com.example.accounting.application.service.finacial;

import com.example.accounting.application.exception.AccountNotFoundException;
import com.example.accounting.application.port.in.AccountUseCase;
import com.example.accounting.application.port.out.AccountRepository;
import com.example.accounting.domain.event.AccountCreatedEvent;
import com.example.accounting.domain.event.AccountDeletedEvent;
import com.example.accounting.domain.event.AccountUpdatedEvent;
import com.example.accounting.domain.model.financial.Account;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 勘定科目サービス（Application Service）
 * Input Port の実装としてビジネスロジックを提供
 */
@Service
@Transactional
public class AccountService implements AccountUseCase {

    private final AccountRepository accountRepository;  // Output Portに依存
    private final ApplicationEventPublisher eventPublisher;

    public AccountService(AccountRepository accountRepository, ApplicationEventPublisher eventPublisher) {
        this.accountRepository = accountRepository;
        this.eventPublisher = eventPublisher;
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

        Account saved = accountRepository.save(account);

        // イベント発行
        Map<String, Object> accountData = new HashMap<>();
        accountData.put("accountCode", saved.getAccountCode());
        accountData.put("accountName", saved.getAccountName());
        accountData.put("bsplType", saved.getBsplType());

        eventPublisher.publishEvent(new AccountCreatedEvent(
            saved.getAccountCode(),
            accountData,
            "system",  // TODO: 認証されたユーザーIDを取得
            "システム", // TODO: 認証されたユーザー名を取得
            null       // TODO: IPアドレスを取得
        ));

        return saved;
    }

    @Override
    public Account updateAccount(String accountCode, Account account) {
        // 存在チェック
        Account oldAccount = getAccountByCode(accountCode);

        // 科目コード変更チェック
        if (!accountCode.equals(account.getAccountCode())) {
            throw new IllegalArgumentException("科目コードは変更できません");
        }

        Account updated = accountRepository.save(account);

        // イベント発行
        Map<String, Object> oldValues = new HashMap<>();
        oldValues.put("accountName", oldAccount.getAccountName());
        oldValues.put("bsplType", oldAccount.getBsplType());

        Map<String, Object> newValues = new HashMap<>();
        newValues.put("accountName", updated.getAccountName());
        newValues.put("bsplType", updated.getBsplType());

        eventPublisher.publishEvent(new AccountUpdatedEvent(
            updated.getAccountCode(),
            oldValues,
            newValues,
            "system",
            "システム",
            null
        ));

        return updated;
    }

    @Override
    public void deleteAccount(String accountCode) {
        // 存在チェック
        Account account = getAccountByCode(accountCode);

        accountRepository.deleteByCode(accountCode);

        // イベント発行
        Map<String, Object> oldValues = new HashMap<>();
        oldValues.put("accountCode", account.getAccountCode());
        oldValues.put("accountName", account.getAccountName());

        eventPublisher.publishEvent(new AccountDeletedEvent(
            account.getAccountCode(),
            oldValues,
            "削除",
            "system",
            "システム",
            null
        ));
    }
}
