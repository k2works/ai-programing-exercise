package com.example.accounting.application.port.in

import com.example.accounting.domain.financial.*
import scalikejdbc.DBSession

import java.time.LocalDate

/**
 * 財務諸表ユースケース（Input Port）
 */
trait FinancialStatementUseCase:

  /** 貸借対照表を生成 */
  def generateBalanceSheet(asOfDate: LocalDate)(implicit session: DBSession): BalanceSheet

  /** 損益計算書を生成 */
  def generateIncomeStatement(fromDate: LocalDate, toDate: LocalDate)(implicit session: DBSession): IncomeStatement

  /** 財務指標を計算 */
  def calculateFinancialRatios(balanceSheet: BalanceSheet, incomeStatement: IncomeStatement): FinancialRatios
