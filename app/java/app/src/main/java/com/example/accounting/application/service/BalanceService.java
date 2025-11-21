package com.example.accounting.application.service;

import com.example.accounting.application.port.out.DailyBalanceRepository;
import com.example.accounting.application.model.DailyBalance;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

/**
 * 残高管理サービス（Application Service）
 */
@Service
@Transactional
public class BalanceService {

    private final DailyBalanceRepository dailyBalanceRepository;

    public BalanceService(DailyBalanceRepository dailyBalanceRepository) {
        this.dailyBalanceRepository = dailyBalanceRepository;
    }

    /**
     * 日次残高を更新（UPSERT）
     *
     * @param balance 日次残高ドメインモデル
     */
    public void updateDailyBalance(DailyBalance balance) {
        dailyBalanceRepository.saveOrUpdate(balance);
    }

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
    @Transactional(readOnly = true)
    public Optional<DailyBalance> getBalance(
            LocalDate entryDate,
            String accountCode,
            String subAccountCode,
            String departmentCode,
            String projectCode,
            boolean isSettlement) {

        return dailyBalanceRepository.findByKey(
                entryDate,
                accountCode,
                subAccountCode,
                departmentCode,
                projectCode,
                isSettlement
        );
    }

    /**
     * 指定日の全残高を取得
     *
     * @param entryDate 起票日
     * @return 日次残高のリスト
     */
    @Transactional(readOnly = true)
    public List<DailyBalance> getBalancesByDate(LocalDate entryDate) {
        return dailyBalanceRepository.findByDate(entryDate);
    }

    /**
     * 指定期間の残高を取得
     *
     * @param startDate 開始日
     * @param endDate 終了日
     * @return 日次残高のリスト
     */
    @Transactional(readOnly = true)
    public List<DailyBalance> getBalancesByDateRange(LocalDate startDate, LocalDate endDate) {
        return dailyBalanceRepository.findByDateRange(startDate, endDate);
    }

    /**
     * 指定勘定科目の残高を取得
     *
     * @param accountCode 勘定科目コード
     * @return 日次残高のリスト
     */
    @Transactional(readOnly = true)
    public List<DailyBalance> getBalancesByAccountCode(String accountCode) {
        return dailyBalanceRepository.findByAccountCode(accountCode);
    }
}
