package com.example.accounting.application.port.in;

import com.example.accounting.domain.model.financial.BalanceSheet;
import com.example.accounting.domain.model.financial.FinancialRatios;
import com.example.accounting.domain.model.financial.IncomeStatement;
import java.time.LocalDate;

/**
 * 財務諸表ユースケース（Input Port）
 * ビジネスユースケースのインターフェース定義
 */
public interface FinancialStatementUseCase {

    /**
     * 貸借対照表を生成
     * @param asOfDate 基準日
     * @return 貸借対照表
     */
    BalanceSheet generateBalanceSheet(LocalDate asOfDate);

    /**
     * 損益計算書を生成
     * @param fromDate 開始日
     * @param toDate 終了日
     * @return 損益計算書
     */
    IncomeStatement generateIncomeStatement(LocalDate fromDate, LocalDate toDate);

    /**
     * 財務指標を計算
     * @param balanceSheet 貸借対照表
     * @param incomeStatement 損益計算書
     * @return 財務指標
     */
    FinancialRatios calculateFinancialRatios(BalanceSheet balanceSheet, IncomeStatement incomeStatement);
}
