package com.example.accounting.application.service

import com.example.accounting.application.port.in.FinancialStatementUseCase
import com.example.accounting.domain.financial.*
import scalikejdbc.*

import java.time.LocalDate
import scala.math.BigDecimal.RoundingMode

/**
 * 財務諸表生成サービス（FinancialStatementUseCase の実装）
 */
class FinancialStatementService extends FinancialStatementUseCase:

  override def generateBalanceSheet(asOfDate: LocalDate)(implicit session: DBSession): BalanceSheet =
    // 資産・負債・純資産の残高を取得
    val balances = sql"""
      SELECT
        a."勘定科目コード" as account_code,
        a."勘定科目名" as account_name,
        a."BSPL区分" as bspl_type,
        a."勘定科目種別"::text as account_type,
        COALESCE(SUM(d."借方金額"), 0) - COALESCE(SUM(d."貸方金額"), 0) as balance
      FROM "勘定科目マスタ" a
      LEFT JOIN "日次勘定科目残高" d
        ON a."勘定科目コード" = d."勘定科目コード"
      WHERE a."BSPL区分" = 'B'
        AND (d."起票日" <= ${asOfDate} OR d."起票日" IS NULL)
      GROUP BY a."勘定科目コード", a."勘定科目名", a."BSPL区分", a."勘定科目種別"
      HAVING COALESCE(SUM(d."借方金額"), 0) - COALESCE(SUM(d."貸方金額"), 0) != 0
      ORDER BY a."勘定科目コード"
    """
      .map(rs =>
        (
          rs.string("account_code"),
          rs.string("account_name"),
          rs.stringOpt("account_type").getOrElse(""),
          BigDecimal(rs.bigDecimal("balance")),
        )
      )
      .list
      .apply()

    // データを分類（勘定科目種別で分類）
    val (assetBalances, liabilityBalances, equityBalances) = balances.foldLeft(
      (
        List.empty[(String, String, BigDecimal)],
        List.empty[(String, String, BigDecimal)],
        List.empty[(String, String, BigDecimal)],
      )
    ) { case ((assets, liabilities, equity), (accountCode, accountName, accountType, balance)) =>
      accountType match
        case "資産" =>
          ((accountCode, accountName, balance.abs) :: assets, liabilities, equity)
        case "負債" =>
          (assets, (accountCode, accountName, balance.abs) :: liabilities, equity)
        case "純資産" | "資本" =>
          (assets, liabilities, (accountCode, accountName, balance.abs) :: equity)
        case _ =>
          (assets, liabilities, equity)
    }

    // 合計を計算
    val totalAssets = assetBalances.map(_._3).sum
    val totalLiabilities = liabilityBalances.map(_._3).sum
    val totalEquity = equityBalances.map(_._3).sum
    val totalLiabilitiesAndEquity = totalLiabilities + totalEquity

    // 構成比率を計算
    val assets = calculatePercentage(assetBalances.reverse, totalAssets)
    val liabilities = calculatePercentage(liabilityBalances.reverse, totalLiabilitiesAndEquity)
    val equity = calculatePercentage(equityBalances.reverse, totalLiabilitiesAndEquity)

    BalanceSheet(
      asOfDate = asOfDate,
      assets = assets,
      liabilities = liabilities,
      equity = equity,
      totalAssets = totalAssets,
      totalLiabilities = totalLiabilities,
      totalEquity = totalEquity,
      totalLiabilitiesAndEquity = totalLiabilitiesAndEquity,
    )

  override def generateIncomeStatement(fromDate: LocalDate, toDate: LocalDate)(implicit
      session: DBSession
  ): IncomeStatement =
    // 収益・費用の残高を取得
    val balances = sql"""
      SELECT
        a."勘定科目コード" as account_code,
        a."勘定科目名" as account_name,
        a."BSPL区分" as bspl_type,
        a."勘定科目種別"::text as account_type,
        COALESCE(SUM(d."貸方金額"), 0) - COALESCE(SUM(d."借方金額"), 0) as balance
      FROM "勘定科目マスタ" a
      LEFT JOIN "日次勘定科目残高" d
        ON a."勘定科目コード" = d."勘定科目コード"
      WHERE a."BSPL区分" = 'P'
        AND d."起票日" BETWEEN ${fromDate} AND ${toDate}
      GROUP BY a."勘定科目コード", a."勘定科目名", a."BSPL区分", a."勘定科目種別"
      HAVING COALESCE(SUM(d."貸方金額"), 0) - COALESCE(SUM(d."借方金額"), 0) != 0
      ORDER BY a."勘定科目コード"
    """
      .map(rs =>
        (
          rs.string("account_code"),
          rs.string("account_name"),
          rs.stringOpt("account_type").getOrElse(""),
          BigDecimal(rs.bigDecimal("balance")),
        )
      )
      .list
      .apply()

    // データを分類（勘定科目種別で分類）
    val (revenueBalances, expenseBalances) = balances.foldLeft(
      (List.empty[(String, String, BigDecimal)], List.empty[(String, String, BigDecimal)])
    ) { case ((revenues, expenses), (accountCode, accountName, accountType, balance)) =>
      accountType match
        case "収益" =>
          ((accountCode, accountName, balance.abs) :: revenues, expenses)
        case "費用" =>
          (revenues, (accountCode, accountName, balance.abs) :: expenses)
        case _ =>
          (revenues, expenses)
    }

    // 合計を計算
    val totalRevenues = revenueBalances.map(_._3).sum
    val totalExpenses = expenseBalances.map(_._3).sum

    // 売上原価（勘定科目コードが51で始まる）
    val costOfSales = expenseBalances
      .filter(_._1.startsWith("51"))
      .map(_._3)
      .sum

    // 販管費（勘定科目コードが6または52で始まる）
    val operatingExpenses = expenseBalances
      .filter(e => e._1.startsWith("6") || e._1.startsWith("52"))
      .map(_._3)
      .sum

    // 利益項目の計算
    val grossProfit = totalRevenues - costOfSales
    val operatingIncome = grossProfit - operatingExpenses
    val netIncome = totalRevenues - totalExpenses

    // 構成比率を計算（対売上比）
    val revenues = calculatePercentageForPL(revenueBalances.reverse, totalRevenues)
    val expenses = calculatePercentageForPL(expenseBalances.reverse, totalRevenues)

    IncomeStatement(
      fromDate = fromDate,
      toDate = toDate,
      revenues = revenues,
      expenses = expenses,
      grossProfit = grossProfit,
      operatingIncome = operatingIncome,
      netIncome = netIncome,
      totalRevenues = totalRevenues,
      totalExpenses = totalExpenses,
    )

  override def calculateFinancialRatios(
      balanceSheet: BalanceSheet,
      incomeStatement: IncomeStatement,
  ): FinancialRatios =
    // 流動資産・流動負債の抽出
    val currentAssets = balanceSheet.assets
      .filter(asset => asset.accountCode.startsWith("11") || asset.accountCode.startsWith("12"))
      .map(_.balance)
      .sum

    val currentLiabilities = balanceSheet.liabilities
      .filter(_.accountCode.startsWith("21"))
      .map(_.balance)
      .sum

    // 各種指標の計算
    val currentRatio = if currentLiabilities > 0 then
      (currentAssets / currentLiabilities * 100).setScale(2, RoundingMode.HALF_UP)
    else BigDecimal(0)

    val equityRatio = if balanceSheet.totalAssets > 0 then
      (balanceSheet.totalEquity / balanceSheet.totalAssets * 100).setScale(2, RoundingMode.HALF_UP)
    else BigDecimal(0)

    val grossProfitMargin = if incomeStatement.totalRevenues > 0 then
      (incomeStatement.grossProfit / incomeStatement.totalRevenues * 100)
        .setScale(2, RoundingMode.HALF_UP)
    else BigDecimal(0)

    val operatingProfitMargin = if incomeStatement.totalRevenues > 0 then
      (incomeStatement.operatingIncome / incomeStatement.totalRevenues * 100)
        .setScale(2, RoundingMode.HALF_UP)
    else BigDecimal(0)

    val netProfitMargin = if incomeStatement.totalRevenues > 0 then
      (incomeStatement.netIncome / incomeStatement.totalRevenues * 100)
        .setScale(2, RoundingMode.HALF_UP)
    else BigDecimal(0)

    val roa = if balanceSheet.totalAssets > 0 then
      (incomeStatement.netIncome / balanceSheet.totalAssets * 100).setScale(2, RoundingMode.HALF_UP)
    else BigDecimal(0)

    val roe = if balanceSheet.totalEquity > 0 then
      (incomeStatement.netIncome / balanceSheet.totalEquity * 100).setScale(2, RoundingMode.HALF_UP)
    else BigDecimal(0)

    FinancialRatios(
      currentRatio = currentRatio,
      equityRatio = equityRatio,
      grossProfitMargin = grossProfitMargin,
      operatingProfitMargin = operatingProfitMargin,
      netProfitMargin = netProfitMargin,
      roa = roa,
      roe = roe,
    )

  private def calculatePercentage(
      items: List[(String, String, BigDecimal)],
      total: BigDecimal,
  ): List[BalanceSheetItem] =
    items.map { case (accountCode, accountName, balance) =>
      val percentage =
        if total > 0 then (balance / total * 100).setScale(2, RoundingMode.HALF_UP)
        else BigDecimal(0)

      BalanceSheetItem(
        accountCode = accountCode,
        accountName = accountName,
        balance = balance,
        percentage = percentage,
      )
    }

  private def calculatePercentageForPL(
      items: List[(String, String, BigDecimal)],
      totalRevenues: BigDecimal,
  ): List[IncomeStatementItem] =
    items.map { case (accountCode, accountName, balance) =>
      val percentage =
        if totalRevenues > 0 then (balance / totalRevenues * 100).setScale(2, RoundingMode.HALF_UP)
        else BigDecimal(0)

      IncomeStatementItem(
        accountCode = accountCode,
        accountName = accountName,
        balance = balance,
        percentage = percentage,
      )
    }
