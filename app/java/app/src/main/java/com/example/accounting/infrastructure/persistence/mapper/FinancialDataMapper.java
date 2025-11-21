package com.example.accounting.infrastructure.persistence.mapper;

import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.time.LocalDate;
import java.util.List;
import java.util.Map;

/**
 * 財務データMapper
 */
@Mapper
public interface FinancialDataMapper {

    /**
     * 貸借対照表の明細項目を取得
     *
     * @param asOfDate 基準日
     * @param elementType 取引要素区分
     * @return 貸借対照表明細のリスト
     */
    List<Map<String, Object>> findBalanceSheetItems(
            @Param("asOfDate") LocalDate asOfDate,
            @Param("elementType") String elementType);

    /**
     * 損益計算書の明細項目を取得
     *
     * @param fromDate 開始日
     * @param toDate 終了日
     * @param elementType 取引要素区分
     * @return 損益計算書明細のリスト
     */
    List<Map<String, Object>> findIncomeStatementItems(
            @Param("fromDate") LocalDate fromDate,
            @Param("toDate") LocalDate toDate,
            @Param("elementType") String elementType);
}
