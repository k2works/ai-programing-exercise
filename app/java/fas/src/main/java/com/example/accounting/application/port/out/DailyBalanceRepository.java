package com.example.accounting.application.port.out;

import com.example.accounting.domain.model.financial.DailyBalance;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

/**
 * 日次残高リポジトリ（Output Port）
 */
public interface DailyBalanceRepository {

    /**
     * 日次残高を保存または更新
     *
     * @param balance 日次残高
     */
    void saveOrUpdate(DailyBalance balance);

    /**
     * 指定条件の残高を取得
     *
     * @param entryDate 起票日
     * @param accountCode 勘定科目コード
     * @param subAccountCode 補助科目コード
     * @param departmentCode 部門コード
     * @param projectCode プロジェクトコード
     * @param isSettlement 決算仕訳フラグ
     * @return 日次残高
     */
    Optional<DailyBalance> findByKey(
            LocalDate entryDate,
            String accountCode,
            String subAccountCode,
            String departmentCode,
            String projectCode,
            boolean isSettlement
    );

    /**
     * 指定日の全残高を取得
     *
     * @param entryDate 起票日
     * @return 日次残高のリスト
     */
    List<DailyBalance> findByDate(LocalDate entryDate);

    /**
     * 指定期間の残高を取得
     *
     * @param startDate 開始日
     * @param endDate 終了日
     * @return 日次残高のリスト
     */
    List<DailyBalance> findByDateRange(LocalDate startDate, LocalDate endDate);

    /**
     * 指定勘定科目の残高を取得
     *
     * @param accountCode 勘定科目コード
     * @return 日次残高のリスト
     */
    List<DailyBalance> findByAccountCode(String accountCode);
}
