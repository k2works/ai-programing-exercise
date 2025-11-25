package com.example.accounting.application.port.out;

import com.example.accounting.domain.model.financial.BalanceSheetItem;
import com.example.accounting.domain.model.financial.IncomeStatementItem;

import java.time.LocalDate;
import java.util.List;

/**
 * 財務データリポジトリ（Output Port）
 */
public interface FinancialDataRepository {

    /**
     * 貸借対照表の明細項目を取得
     *
     * @param asOfDate 基準日
     * @param elementType 取引要素区分（'1'=資産、'2'=負債、'3'=純資産）
     * @return 貸借対照表明細のリスト
     */
    List<BalanceSheetItem> findBalanceSheetItems(LocalDate asOfDate, String elementType);

    /**
     * 損益計算書の明細項目を取得
     *
     * @param fromDate 開始日
     * @param toDate 終了日
     * @param elementType 取引要素区分（'4'=収益、'5'=費用）
     * @return 損益計算書明細のリスト
     */
    List<IncomeStatementItem> findIncomeStatementItems(
            LocalDate fromDate, LocalDate toDate, String elementType);
}
