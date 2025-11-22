package com.example.accounting.infrastructure.out.persistence.mapper;

import com.example.accounting.infrastructure.out.persistence.dao.DailyAccountBalance;
import org.apache.ibatis.annotations.Mapper;

import java.time.LocalDate;
import java.util.List;

/**
 * 日次勘定科目残高 Mapper
 */
@Mapper
public interface DailyAccountBalanceMapper {

    /**
     * 日次残高を挿入または更新（UPSERT）
     *
     * @param balance 日次残高エンティティ
     */
    void upsert(DailyAccountBalance balance);

    /**
     * 指定日の残高を取得
     *
     * @param entryDate 起票日
     * @param accountCode 勘定科目コード
     * @param subAccountCode 補助科目コード
     * @param departmentCode 部門コード
     * @param projectCode プロジェクトコード
     * @param settlementFlag 決算仕訳フラグ
     * @return 日次残高エンティティ
     */
    DailyAccountBalance findByKey(
            LocalDate entryDate,
            String accountCode,
            String subAccountCode,
            String departmentCode,
            String projectCode,
            Integer settlementFlag
    );

    /**
     * 指定日の全残高を取得
     *
     * @param entryDate 起票日
     * @return 日次残高エンティティのリスト
     */
    List<DailyAccountBalance> findByDate(LocalDate entryDate);

    /**
     * 指定期間の残高を取得
     *
     * @param startDate 開始日
     * @param endDate 終了日
     * @return 日次残高エンティティのリスト
     */
    List<DailyAccountBalance> findByDateRange(LocalDate startDate, LocalDate endDate);

    /**
     * 指定勘定科目の残高を取得
     *
     * @param accountCode 勘定科目コード
     * @return 日次残高エンティティのリスト
     */
    List<DailyAccountBalance> findByAccountCode(String accountCode);
}
