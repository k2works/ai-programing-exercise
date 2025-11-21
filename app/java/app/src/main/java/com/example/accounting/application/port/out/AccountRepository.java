package com.example.accounting.application.port.out;

import com.example.accounting.application.model.Account;
import java.util.List;
import java.util.Optional;

/**
 * 勘定科目リポジトリ（Output Port）
 * データアクセスの抽象化
 */
public interface AccountRepository {

    /**
     * すべての勘定科目を取得
     * @return 勘定科目リスト
     */
    List<Account> findAll();

    /**
     * 科目コードで勘定科目を検索
     * @param accountCode 科目コード
     * @return 勘定科目（存在しない場合は Empty）
     */
    Optional<Account> findByCode(String accountCode);

    /**
     * BSPL区分で勘定科目を検索
     * @param bsplType BSPL区分（'B' または 'P'）
     * @return 勘定科目リスト
     */
    List<Account> findByBsplType(String bsplType);

    /**
     * 勘定科目を保存（作成または更新）
     * @param account 勘定科目
     * @return 保存された勘定科目
     */
    Account save(Account account);

    /**
     * 勘定科目を削除
     * @param accountCode 科目コード
     */
    void deleteByCode(String accountCode);
}
