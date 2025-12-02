package com.example.accounting.domain.analysis

/**
 * 財務指標を計算するドメインロジック
 * すべてのメソッドは純粋関数として実装
 */
object FinancialRatioAnalyzer:

  def analyze(data: FinancialData): Either[AnalysisError, AnalysisResult] =
    for
      profitability <- calculateProfitability(data)
      efficiency <- calculateEfficiency(data)
      safety <- calculateSafety(data)
    yield AnalysisResult(data.fiscalYear, profitability, efficiency, safety)

  /**
   * 複数年度の財務データを分析・比較
   *
   * @param dataList 財務データのリスト（年度順）
   * @return 比較結果
   */
  def compare(dataList: List[FinancialData]): Either[AnalysisError, ComparisonResult] =
    // 各年度を分析
    val results = dataList.map(analyze)
    val errors = results.collect { case Left(e) => e }
    if errors.nonEmpty then
      Left(errors.head)
    else
      val analysisResults = results.collect { case Right(r) => r }
      val sortedResults = analysisResults.sortBy(_.fiscalYear)

      // 2年度以上ある場合は変化率を計算
      val changes = if sortedResults.size >= 2 then
        val first = sortedResults.head
        val last = sortedResults.last
        Some(calculateChanges(first, last))
      else
        None

      Right(ComparisonResult(sortedResults, changes))

  /**
   * 2年度間の指標変化を計算
   */
  private def calculateChanges(from: AnalysisResult, to: AnalysisResult): RatioChanges =
    RatioChanges(
      profitability = ProfitabilityChange(
        grossProfitMarginChange = to.profitability.grossProfitMargin - from.profitability.grossProfitMargin,
        operatingProfitMarginChange = to.profitability.operatingProfitMargin - from.profitability.operatingProfitMargin,
        ordinaryProfitMarginChange = to.profitability.ordinaryProfitMargin - from.profitability.ordinaryProfitMargin,
        sellingExpenseRatioChange = to.profitability.sellingExpenseRatio - from.profitability.sellingExpenseRatio
      ),
      efficiency = EfficiencyChange(
        totalAssetTurnoverChange = to.efficiency.totalAssetTurnover - from.efficiency.totalAssetTurnover,
        fixedAssetTurnoverChange = to.efficiency.fixedAssetTurnover - from.efficiency.fixedAssetTurnover
      ),
      safety = SafetyChange(
        currentRatioChange = to.safety.currentRatio - from.safety.currentRatio,
        equityRatioChange = to.safety.equityRatio - from.safety.equityRatio,
        debtRatioChange = to.safety.debtRatio - from.safety.debtRatio
      )
    )

  /**
   * 収益性指標を計算
   */
  private def calculateProfitability(data: FinancialData): Either[AnalysisError, Profitability] =
    for
      grossProfitMargin <- calculateRatio(data.grossProfit, data.sales, "売上高総利益率")
      operatingProfitMargin <- calculateRatio(data.operatingProfit, data.sales, "売上高営業利益率")
      ordinaryProfitMargin <- calculateRatio(data.ordinaryProfit, data.sales, "売上高経常利益率")
      sellingExpenseRatio <- calculateRatio(data.sellingGeneralExpenses, data.sales, "売上高販管費比率")
    yield Profitability(
      grossProfitMargin = grossProfitMargin,
      operatingProfitMargin = operatingProfitMargin,
      ordinaryProfitMargin = ordinaryProfitMargin,
      sellingExpenseRatio = sellingExpenseRatio
    )

  /**
   * 効率性指標を計算
   */
  private def calculateEfficiency(data: FinancialData): Either[AnalysisError, Efficiency] =
    for
      totalAssetTurnover <- calculateTurnover(data.sales, data.totalAssets, "総資本回転率")
      fixedAssetTurnover <- calculateTurnover(data.sales, data.fixedAssets, "有形固定資産回転率")
    yield Efficiency(
      totalAssetTurnover = totalAssetTurnover,
      fixedAssetTurnover = fixedAssetTurnover
    )

  /**
   * 安全性指標を計算
   */
  private def calculateSafety(data: FinancialData): Either[AnalysisError, Safety] =
    for
      currentRatio <- calculateRatio(data.currentAssets, data.currentLiabilities, "流動比率")
      equityRatio <- calculateRatio(data.equity, data.totalAssets, "自己資本比率")
      debtRatio <- calculateRatio(data.totalLiabilities, data.equity, "負債比率")
    yield Safety(
      currentRatio = currentRatio,
      equityRatio = equityRatio,
      debtRatio = debtRatio
    )

  /**
   * 比率を計算するヘルパー関数
   */
  private def calculateRatio(
    numerator: BigDecimal,
    denominator: BigDecimal,
    name: String
  ): Either[AnalysisError, BigDecimal] =
    if denominator <= 0 then
      Left(AnalysisError.InvalidDenominator(s"$name の計算で分母がゼロまたは負です"))
    else
      Right(
        (numerator / denominator * 100)
          .setScale(2, BigDecimal.RoundingMode.HALF_UP)
      )

  /**
   * 回転率を計算するヘルパー関数
   */
  private def calculateTurnover(
    sales: BigDecimal,
    asset: BigDecimal,
    name: String
  ): Either[AnalysisError, BigDecimal] =
    if asset <= 0 then
      Left(AnalysisError.InvalidAsset(s"$name の計算で資産がゼロまたは負です"))
    else
      Right(
        (sales / asset)
          .setScale(2, BigDecimal.RoundingMode.HALF_UP)
      )

/**
 * 収益性指標
 *
 * @param grossProfitMargin 売上高総利益率（%）
 * @param operatingProfitMargin 売上高営業利益率（%）
 * @param ordinaryProfitMargin 売上高経常利益率（%）
 * @param sellingExpenseRatio 売上高販管費比率（%）
 */
case class Profitability(
  grossProfitMargin: BigDecimal,
  operatingProfitMargin: BigDecimal,
  ordinaryProfitMargin: BigDecimal,
  sellingExpenseRatio: BigDecimal
)

/**
 * 効率性指標
 *
 * @param totalAssetTurnover 総資本回転率（回）
 * @param fixedAssetTurnover 有形固定資産回転率（回）
 */
case class Efficiency(
  totalAssetTurnover: BigDecimal,
  fixedAssetTurnover: BigDecimal
)

/**
 * 安全性指標
 *
 * @param currentRatio 流動比率（%）
 * @param equityRatio 自己資本比率（%）
 * @param debtRatio 負債比率（%）
 */
case class Safety(
  currentRatio: BigDecimal,
  equityRatio: BigDecimal,
  debtRatio: BigDecimal
)

/**
 * 分析結果
 *
 * @param fiscalYear 会計年度
 * @param profitability 収益性指標
 * @param efficiency 効率性指標
 * @param safety 安全性指標
 */
case class AnalysisResult(
  fiscalYear: Int,
  profitability: Profitability,
  efficiency: Efficiency,
  safety: Safety
)

/**
 * 複数年度の比較結果
 *
 * @param results 年度ごとの分析結果
 * @param changes 指標の変化率
 */
case class ComparisonResult(
  results: List[AnalysisResult],
  changes: Option[RatioChanges]
)

/**
 * 指標の変化率
 *
 * @param profitability 収益性指標の変化
 * @param efficiency 効率性指標の変化
 * @param safety 安全性指標の変化
 */
case class RatioChanges(
  profitability: ProfitabilityChange,
  efficiency: EfficiencyChange,
  safety: SafetyChange
)

/**
 * 収益性指標の変化
 */
case class ProfitabilityChange(
  grossProfitMarginChange: BigDecimal,
  operatingProfitMarginChange: BigDecimal,
  ordinaryProfitMarginChange: BigDecimal,
  sellingExpenseRatioChange: BigDecimal
)

/**
 * 効率性指標の変化
 */
case class EfficiencyChange(
  totalAssetTurnoverChange: BigDecimal,
  fixedAssetTurnoverChange: BigDecimal
)

/**
 * 安全性指標の変化
 */
case class SafetyChange(
  currentRatioChange: BigDecimal,
  equityRatioChange: BigDecimal,
  debtRatioChange: BigDecimal
)

/**
 * 分析エラー
 */
sealed trait AnalysisError:
  def message: String

object AnalysisError:
  case class InvalidDenominator(message: String) extends AnalysisError
  case class InvalidAsset(message: String) extends AnalysisError
