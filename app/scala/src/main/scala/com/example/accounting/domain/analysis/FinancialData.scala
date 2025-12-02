package com.example.accounting.domain.analysis

/**
 * 財務データを表現する値オブジェクト
 *
 * @param fiscalYear 会計年度
 * @param sales 売上高
 * @param costOfSales 売上原価
 * @param grossProfit 売上総利益
 * @param sellingGeneralExpenses 販売費及び一般管理費
 * @param operatingProfit 営業利益
 * @param nonOperatingIncome 営業外収益
 * @param nonOperatingExpenses 営業外費用
 * @param ordinaryProfit 経常利益
 * @param currentAssets 流動資産
 * @param fixedAssets 固定資産
 * @param totalAssets 総資産
 * @param currentLiabilities 流動負債
 * @param fixedLiabilities 固定負債
 * @param totalLiabilities 負債合計
 * @param equity 純資産
 */
case class FinancialData(
  fiscalYear: Int,
  sales: BigDecimal,
  costOfSales: BigDecimal,
  grossProfit: BigDecimal,
  sellingGeneralExpenses: BigDecimal,
  operatingProfit: BigDecimal,
  nonOperatingIncome: BigDecimal,
  nonOperatingExpenses: BigDecimal,
  ordinaryProfit: BigDecimal,
  currentAssets: BigDecimal,
  fixedAssets: BigDecimal,
  totalAssets: BigDecimal,
  currentLiabilities: BigDecimal,
  fixedLiabilities: BigDecimal,
  totalLiabilities: BigDecimal,
  equity: BigDecimal
)

object FinancialData:

  /**
   * 勘定科目残高データから財務データを構築
   *
   * @param fiscalYear 会計年度
   * @param balances 勘定科目コード -> (借方金額, 貸方金額) のマップ
   * @return 財務データまたはエラー
   */
  def fromBalances(
    fiscalYear: Int,
    balances: Map[String, (BigDecimal, BigDecimal)]
  ): Either[String, FinancialData] =

    def getBalance(code: String): Either[String, BigDecimal] =
      balances.get(code) match
        case Some((debit, credit)) => Right(debit - credit)
        case None => Left(s"勘定科目コード $code が見つかりません")

    def getAbsBalance(code: String): Either[String, BigDecimal] =
      balances.get(code) match
        case Some((debit, credit)) => Right((debit - credit).abs)
        case None => Left(s"勘定科目コード $code が見つかりません")

    for
      // 収益・費用（絶対値で取得）
      sales <- getAbsBalance("41")
      costOfSales <- getAbsBalance("51")
      sellingGeneralExpenses <- getAbsBalance("52")
      nonOperatingIncome <- getAbsBalance("42")
      nonOperatingExpenses <- getAbsBalance("53")

      // 資産・負債・純資産（絶対値で取得）
      currentAssets <- getAbsBalance("11")
      fixedAssets <- getAbsBalance("12")
      currentLiabilities <- getAbsBalance("21")
      fixedLiabilities <- getAbsBalance("22")
      equity <- getAbsBalance("3").orElse {
        // 純資産が集計科目でない場合は個別科目から計算
        for
          capital <- getAbsBalance("31")
          retainedEarnings <- getAbsBalance("33")
        yield capital + retainedEarnings
      }
    yield
      val grossProfit = sales - costOfSales
      val operatingProfit = grossProfit - sellingGeneralExpenses
      val ordinaryProfit = operatingProfit + nonOperatingIncome - nonOperatingExpenses
      val totalAssets = currentAssets + fixedAssets
      val totalLiabilities = currentLiabilities + fixedLiabilities

      FinancialData(
        fiscalYear = fiscalYear,
        sales = sales,
        costOfSales = costOfSales,
        grossProfit = grossProfit,
        sellingGeneralExpenses = sellingGeneralExpenses,
        operatingProfit = operatingProfit,
        nonOperatingIncome = nonOperatingIncome,
        nonOperatingExpenses = nonOperatingExpenses,
        ordinaryProfit = ordinaryProfit,
        currentAssets = currentAssets,
        fixedAssets = fixedAssets,
        totalAssets = totalAssets,
        currentLiabilities = currentLiabilities,
        fixedLiabilities = fixedLiabilities,
        totalLiabilities = totalLiabilities,
        equity = equity
      )

  /**
   * D社の令和3年度財務データを取得
   */
  def fy2021Data: FinancialData = FinancialData(
    fiscalYear = 2021,
    sales = BigDecimal("5796105000"),
    costOfSales = BigDecimal("2185856000"),
    grossProfit = BigDecimal("3610249000"),
    sellingGeneralExpenses = BigDecimal("2625222000"),
    operatingProfit = BigDecimal("985027000"),
    nonOperatingIncome = BigDecimal("368000"),
    nonOperatingExpenses = BigDecimal("2676000"),
    ordinaryProfit = BigDecimal("982719000"),
    currentAssets = BigDecimal("2676193000"),
    fixedAssets = BigDecimal("186973000"),
    totalAssets = BigDecimal("2863166000"),
    currentLiabilities = BigDecimal("851394000"),
    fixedLiabilities = BigDecimal("22500000"),
    totalLiabilities = BigDecimal("873894000"),
    equity = BigDecimal("1989272000")
  )

  /**
   * D社の令和4年度財務データを取得
   */
  def fy2022Data: FinancialData = FinancialData(
    fiscalYear = 2022,
    sales = BigDecimal("4547908000"),
    costOfSales = BigDecimal("1743821000"),
    grossProfit = BigDecimal("2804087000"),
    sellingGeneralExpenses = BigDecimal("2277050000"),
    operatingProfit = BigDecimal("527037000"),
    nonOperatingIncome = BigDecimal("11608000"),
    nonOperatingExpenses = BigDecimal("1613000"),
    ordinaryProfit = BigDecimal("537032000"),
    currentAssets = BigDecimal("2777545000"),
    fixedAssets = BigDecimal("197354000"),
    totalAssets = BigDecimal("2974899000"),
    currentLiabilities = BigDecimal("640513000"),
    fixedLiabilities = BigDecimal("27153000"),
    totalLiabilities = BigDecimal("667666000"),
    equity = BigDecimal("2307233000")
  )
