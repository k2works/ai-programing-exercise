package com.example.accounting.infrastructure.out.persistence.mapper;

import com.example.accounting.infrastructure.out.persistence.entity.Account;
import org.apache.ibatis.annotations.Param;

import java.math.BigDecimal;
import java.util.List;

/**
 * 勘定科目マスタ Mapper インターフェース
 */
public interface AccountMapper {

    /**
     * 勘定科目を登録する
     *
     * @param account 勘定科目
     */
    void insert(Account account);

    /**
     * 勘定科目コードで検索する
     *
     * @param accountCode 勘定科目コード
     * @return 勘定科目
     */
    Account findByCode(@Param("accountCode") String accountCode);

    /**
     * 勘定科目IDで検索する
     *
     * @param accountId 勘定科目ID
     * @return 勘定科目
     */
    Account findById(@Param("accountId") Integer accountId);

    /**
     * 全ての勘定科目を取得する
     *
     * @return 勘定科目リスト
     */
    List<Account> findAll();

    /**
     * 勘定科目種別で検索する
     *
     * @param accountType 勘定科目種別
     * @return 勘定科目リスト
     */
    List<Account> findByType(@Param("accountType") String accountType);

    /**
     * BSPL区分で検索する
     *
     * @param bsplType BSPL区分
     * @return 勘定科目リスト
     */
    List<Account> findByBsplType(@Param("bsplType") String bsplType);

    /**
     * 勘定科目コードで検索する（findByCodeのエイリアス）
     *
     * @param accountCode 勘定科目コード
     * @return 勘定科目
     */
    Account findByAccountCode(@Param("accountCode") String accountCode);

    /**
     * 合計科目のみ取得する
     *
     * @return 合計科目リスト
     */
    List<Account> findSummaryAccounts();

    /**
     * 明細科目のみ取得する
     *
     * @return 明細科目リスト
     */
    List<Account> findDetailAccounts();

    /**
     * 勘定科目を更新する
     *
     * @param account 勘定科目
     */
    void update(Account account);

    /**
     * 勘定科目の残高を更新する
     *
     * @param accountCode 勘定科目コード
     * @param balance 残高
     */
    void updateBalance(@Param("accountCode") String accountCode, @Param("balance") BigDecimal balance);

    /**
     * 勘定科目を削除する
     *
     * @param accountCode 勘定科目コード
     */
    void delete(@Param("accountCode") String accountCode);
}
