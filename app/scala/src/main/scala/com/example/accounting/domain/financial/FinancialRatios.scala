package com.example.accounting.domain.financial

/**
 * 財務指標
 *
 * @param currentRatio 流動比率（％）
 * @param equityRatio 自己資本比率（％）
 * @param grossProfitMargin 売上総利益率（％）
 * @param operatingProfitMargin 営業利益率（％）
 * @param netProfitMargin 当期純利益率（％）
 * @param roa 総資産利益率（ROA, ％）
 * @param roe 自己資本利益率（ROE, ％）
 */
case class FinancialRatios(
    currentRatio: BigDecimal,
    equityRatio: BigDecimal,
    grossProfitMargin: BigDecimal,
    operatingProfitMargin: BigDecimal,
    netProfitMargin: BigDecimal,
    roa: BigDecimal,
    roe: BigDecimal,
)
