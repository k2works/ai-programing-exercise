package com.example.accounting.application.port.in;

import com.example.accounting.domain.model.Account;
import java.util.List;

/**
 * 勘定科目ユースケース（Input Port）
 * ビジネスユースケースのインターフェース定義
 */
public interface AccountUseCase {

    /**
     * すべての勘定科目を取得
     * @return 勘定科目一覧
     */
    List<Account> getAllAccounts();

    /**
     * 科目コードで勘定科目を取得
     * @param accountCode 科目コード
     * @return 勘定科目
     */
    Account getAccountByCode(String accountCode);

    /**
     * BSPL区分で勘定科目を取得
     * @param bsplType BSPL区分（'B' または 'P'）
     * @return 勘定科目リスト
     */
    List<Account> getAccountsByBsplType(String bsplType);

    /**
     * 勘定科目を作成
     * @param account 勘定科目
     * @return 作成された勘定科目
     */
    Account createAccount(Account account);

    /**
     * 勘定科目を更新
     * @param accountCode 科目コード
     * @param account 勘定科目
     * @return 更新された勘定科目
     */
    Account updateAccount(String accountCode, Account account);

    /**
     * 勘定科目を削除
     * @param accountCode 科目コード
     */
    void deleteAccount(String accountCode);
}
