package com.example.accounting.domain.financial

import java.time.LocalDate

/**
 * 損益計算書（Income Statement / P/L）
 *
 * @param fromDate 開始日
 * @param toDate 終了日
 * @param revenues 収益項目
 * @param expenses 費用項目
 * @param grossProfit 売上総利益
 * @param operatingIncome 営業利益
 * @param netIncome 当期純利益
 * @param totalRevenues 総収益
 * @param totalExpenses 総費用
 */
case class IncomeStatement(
    fromDate: LocalDate,
    toDate: LocalDate,
    revenues: List[IncomeStatementItem],
    expenses: List[IncomeStatementItem],
    grossProfit: BigDecimal,
    operatingIncome: BigDecimal,
    netIncome: BigDecimal,
    totalRevenues: BigDecimal,
    totalExpenses: BigDecimal,
)
