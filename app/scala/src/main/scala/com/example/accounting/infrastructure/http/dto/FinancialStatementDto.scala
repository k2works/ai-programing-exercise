package com.example.accounting.infrastructure.http.dto

import com.example.accounting.domain.financial.*
import java.time.LocalDate

/**
 * 財務諸表項目レスポンス DTO
 */
case class FinancialStatementItemResponse(
    accountCode: String,
    accountName: String,
    balance: BigDecimal,
    percentage: BigDecimal,
)

/**
 * 貸借対照表レスポンス DTO
 */
case class BalanceSheetResponse(
    asOfDate: LocalDate,
    assets: List[FinancialStatementItemResponse],
    liabilities: List[FinancialStatementItemResponse],
    equity: List[FinancialStatementItemResponse],
    totalAssets: BigDecimal,
    totalLiabilities: BigDecimal,
    totalEquity: BigDecimal,
    totalLiabilitiesAndEquity: BigDecimal,
    isBalanced: Boolean,
)

object BalanceSheetResponse:
  def fromDomain(bs: BalanceSheet): BalanceSheetResponse =
    BalanceSheetResponse(
      asOfDate = bs.asOfDate,
      assets = bs.assets.map(item =>
        FinancialStatementItemResponse(
          accountCode = item.accountCode,
          accountName = item.accountName,
          balance = item.balance,
          percentage = item.percentage,
        )
      ),
      liabilities = bs.liabilities.map(item =>
        FinancialStatementItemResponse(
          accountCode = item.accountCode,
          accountName = item.accountName,
          balance = item.balance,
          percentage = item.percentage,
        )
      ),
      equity = bs.equity.map(item =>
        FinancialStatementItemResponse(
          accountCode = item.accountCode,
          accountName = item.accountName,
          balance = item.balance,
          percentage = item.percentage,
        )
      ),
      totalAssets = bs.totalAssets,
      totalLiabilities = bs.totalLiabilities,
      totalEquity = bs.totalEquity,
      totalLiabilitiesAndEquity = bs.totalLiabilitiesAndEquity,
      isBalanced = bs.isBalanced,
    )

/**
 * 損益計算書レスポンス DTO
 */
case class IncomeStatementResponse(
    fromDate: LocalDate,
    toDate: LocalDate,
    revenues: List[FinancialStatementItemResponse],
    expenses: List[FinancialStatementItemResponse],
    grossProfit: BigDecimal,
    operatingIncome: BigDecimal,
    netIncome: BigDecimal,
    totalRevenues: BigDecimal,
    totalExpenses: BigDecimal,
)

object IncomeStatementResponse:
  def fromDomain(is: IncomeStatement): IncomeStatementResponse =
    IncomeStatementResponse(
      fromDate = is.fromDate,
      toDate = is.toDate,
      revenues = is.revenues.map(item =>
        FinancialStatementItemResponse(
          accountCode = item.accountCode,
          accountName = item.accountName,
          balance = item.balance,
          percentage = item.percentage,
        )
      ),
      expenses = is.expenses.map(item =>
        FinancialStatementItemResponse(
          accountCode = item.accountCode,
          accountName = item.accountName,
          balance = item.balance,
          percentage = item.percentage,
        )
      ),
      grossProfit = is.grossProfit,
      operatingIncome = is.operatingIncome,
      netIncome = is.netIncome,
      totalRevenues = is.totalRevenues,
      totalExpenses = is.totalExpenses,
    )

/**
 * 財務指標レスポンス DTO
 */
case class FinancialRatiosResponse(
    currentRatio: BigDecimal,
    equityRatio: BigDecimal,
    grossProfitMargin: BigDecimal,
    operatingProfitMargin: BigDecimal,
    netProfitMargin: BigDecimal,
    roa: BigDecimal,
    roe: BigDecimal,
)

object FinancialRatiosResponse:
  def fromDomain(fr: FinancialRatios): FinancialRatiosResponse =
    FinancialRatiosResponse(
      currentRatio = fr.currentRatio,
      equityRatio = fr.equityRatio,
      grossProfitMargin = fr.grossProfitMargin,
      operatingProfitMargin = fr.operatingProfitMargin,
      netProfitMargin = fr.netProfitMargin,
      roa = fr.roa,
      roe = fr.roe,
    )
