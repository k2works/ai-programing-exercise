package com.example.accounting.domain.financial

/**
 * 損益計算書の項目（勘定科目ごとの金額と構成比率）
 *
 * @param accountCode 勘定科目コード
 * @param accountName 勘定科目名
 * @param balance 金額
 * @param percentage 対売上比（％）
 */
case class IncomeStatementItem(
    accountCode: String,
    accountName: String,
    balance: BigDecimal,
    percentage: BigDecimal,
)
