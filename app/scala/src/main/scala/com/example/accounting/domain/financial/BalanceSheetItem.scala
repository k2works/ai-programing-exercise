package com.example.accounting.domain.financial

/**
 * 貸借対照表の項目（勘定科目ごとの残高と構成比率）
 *
 * @param accountCode 勘定科目コード
 * @param accountName 勘定科目名
 * @param balance 残高
 * @param percentage 構成比率（％）
 */
case class BalanceSheetItem(
    accountCode: String,
    accountName: String,
    balance: BigDecimal,
    percentage: BigDecimal,
)
