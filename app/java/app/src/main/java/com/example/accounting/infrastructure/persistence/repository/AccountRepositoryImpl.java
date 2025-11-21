package com.example.accounting.infrastructure.persistence.repository;

import com.example.accounting.application.port.out.AccountRepository;
import com.example.accounting.domain.model.Account;
import com.example.accounting.infrastructure.persistence.mapper.AccountMapper;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * 勘定科目リポジトリ実装（Output Adapter）
 * MyBatis Mapperとドメインモデルの橋渡し
 */
@Repository
public class AccountRepositoryImpl implements AccountRepository {

    private final AccountMapper accountMapper;

    public AccountRepositoryImpl(AccountMapper accountMapper) {
        this.accountMapper = accountMapper;
    }

    @Override
    public List<Account> findAll() {
        return accountMapper.findAll().stream()
                .map(this::toDomainModel)
                .collect(Collectors.toList());
    }

    @Override
    public Optional<Account> findByCode(String accountCode) {
        com.example.accounting.infrastructure.persistence.entity.Account entity = accountMapper.findByAccountCode(accountCode);
        return Optional.ofNullable(entity).map(this::toDomainModel);
    }

    @Override
    public List<Account> findByBsplType(String bsplType) {
        return accountMapper.findByBsplType(bsplType).stream()
                .map(this::toDomainModel)
                .collect(Collectors.toList());
    }

    @Override
    public Account save(Account account) {
        com.example.accounting.infrastructure.persistence.entity.Account entity = accountMapper.findByAccountCode(account.getAccountCode());

        if (entity == null) {
            // 新規作成
            entity = toEntity(account);
            accountMapper.insert(entity);
        } else {
            // 更新
            entity = toEntity(account);
            entity.setAccountId(accountMapper.findByAccountCode(account.getAccountCode()).getAccountId());
            accountMapper.update(entity);
        }

        return toDomainModel(entity);
    }

    @Override
    public void deleteByCode(String accountCode) {
        accountMapper.delete(accountCode);
    }

    /**
     * EntityからDomain Modelへ変換
     */
    private Account toDomainModel(com.example.accounting.infrastructure.persistence.entity.Account entity) {
        if (entity == null) {
            return null;
        }

        Account domain = new Account();
        domain.setAccountCode(entity.getAccountCode());
        domain.setAccountName(entity.getAccountName());
        domain.setAccountAbbr(entity.getAccountName()); // 簡略名は同じものを使用
        domain.setAccountKana(entity.getAccountNameKana());
        domain.setBsplType(entity.getBsplType());
        domain.setDebitCreditType(mapAccountTypeToDebitCredit(entity.getAccountType()));
        domain.setElementType(entity.getTransactionElementType());
        domain.setAggregationType(entity.getIsAggregationTarget() ? "1" : "0");
        domain.setDisplayOrder(entity.getDisplayOrder());

        return domain;
    }

    /**
     * Domain ModelからEntityへ変換
     */
    private com.example.accounting.infrastructure.persistence.entity.Account toEntity(Account domain) {
        com.example.accounting.infrastructure.persistence.entity.Account entity = new com.example.accounting.infrastructure.persistence.entity.Account();
        entity.setAccountCode(domain.getAccountCode());
        entity.setAccountName(domain.getAccountName());
        entity.setAccountNameKana(domain.getAccountKana());
        entity.setAccountType(mapDebitCreditToAccountType(domain.getDebitCreditType()));
        entity.setBsplType(domain.getBsplType());
        entity.setTransactionElementType(domain.getElementType());
        entity.setIsAggregationTarget("1".equals(domain.getAggregationType()));
        entity.setDisplayOrder(domain.getDisplayOrder());
        entity.setIsSummaryAccount(false);

        return entity;
    }

    /**
     * 勘定科目種別から貸借区分へのマッピング
     */
    private String mapAccountTypeToDebitCredit(String accountType) {
        if (accountType == null) {
            return null;
        }
        return switch (accountType) {
            case "資産", "費用" -> "借";
            case "負債", "純資産", "収益" -> "貸";
            default -> null;
        };
    }

    /**
     * 貸借区分から勘定科目種別へのマッピング
     */
    private String mapDebitCreditToAccountType(String debitCreditType) {
        if (debitCreditType == null) {
            return "資産"; // デフォルト
        }
        return "借".equals(debitCreditType) ? "資産" : "負債";
    }
}
