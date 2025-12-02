package com.example.management.adapter.api

import cats.effect.IO
import cats.implicits.*
import org.http4s.HttpRoutes
import sttp.tapir.*
import sttp.tapir.server.http4s.Http4sServerInterpreter
import sttp.tapir.json.circe.*
import sttp.tapir.generic.auto.*
import io.circe.generic.auto.*
import com.example.management.application.FinancialAnalysisService
import com.example.management.domain.*
import com.example.common.domain.*
import com.example.common.json.JsonCodecs.given

/**
 * 分析結果 DTO
 */
case class AnalysisResultDto(
    fiscalYear: Int,
    profitability: ProfitabilityDto,
    efficiency: EfficiencyDto,
    safety: SafetyDto
)

case class ProfitabilityDto(
    grossProfitMargin: BigDecimal,
    operatingProfitMargin: BigDecimal,
    ordinaryProfitMargin: BigDecimal,
    sellingExpenseRatio: BigDecimal
)

case class EfficiencyDto(
    totalAssetTurnover: BigDecimal,
    fixedAssetTurnover: BigDecimal
)

case class SafetyDto(
    currentRatio: BigDecimal,
    equityRatio: BigDecimal,
    debtRatio: BigDecimal
)

case class ComparisonResultDto(
    results: List[AnalysisResultDto],
    trends: TrendAnalysisDto
)

case class TrendAnalysisDto(
    salesGrowthRate: BigDecimal,
    profitGrowthRate: BigDecimal,
    operatingProfitMarginTrend: String,
    equityRatioTrend: String
)

case class FinancialDataDto(
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

case class ErrorResponse(
    errorType: String,
    message: String
)

object AnalysisResultDto:
  def fromDomain(result: AnalysisResult): AnalysisResultDto =
    AnalysisResultDto(
      fiscalYear = result.fiscalYear.value,
      profitability = ProfitabilityDto(
        grossProfitMargin = result.profitability.grossProfitMargin,
        operatingProfitMargin = result.profitability.operatingProfitMargin,
        ordinaryProfitMargin = result.profitability.ordinaryProfitMargin,
        sellingExpenseRatio = result.profitability.sellingExpenseRatio
      ),
      efficiency = EfficiencyDto(
        totalAssetTurnover = result.efficiency.totalAssetTurnover,
        fixedAssetTurnover = result.efficiency.fixedAssetTurnover
      ),
      safety = SafetyDto(
        currentRatio = result.safety.currentRatio,
        equityRatio = result.safety.equityRatio,
        debtRatio = result.safety.debtRatio
      )
    )

object ComparisonResultDto:
  def fromDomain(result: ComparisonResult): ComparisonResultDto =
    ComparisonResultDto(
      results = result.results.map(AnalysisResultDto.fromDomain),
      trends = TrendAnalysisDto(
        salesGrowthRate = result.trends.salesGrowthRate,
        profitGrowthRate = result.trends.profitGrowthRate,
        operatingProfitMarginTrend = result.trends.operatingProfitMarginTrend,
        equityRatioTrend = result.trends.equityRatioTrend
      )
    )

object FinancialDataDto:
  def fromDomain(data: FinancialData): FinancialDataDto =
    FinancialDataDto(
      fiscalYear = data.fiscalYear.value,
      sales = data.sales.value,
      costOfSales = data.costOfSales.value,
      grossProfit = data.grossProfit.value,
      sellingGeneralExpenses = data.sellingGeneralExpenses.value,
      operatingProfit = data.operatingProfit.value,
      nonOperatingIncome = data.nonOperatingIncome.value,
      nonOperatingExpenses = data.nonOperatingExpenses.value,
      ordinaryProfit = data.ordinaryProfit.value,
      currentAssets = data.currentAssets.value,
      fixedAssets = data.fixedAssets.value,
      totalAssets = data.totalAssets.value,
      currentLiabilities = data.currentLiabilities.value,
      fixedLiabilities = data.fixedLiabilities.value,
      totalLiabilities = data.totalLiabilities.value,
      equity = data.equity.value
    )

/**
 * 財務分析 API エンドポイント
 */
class AnalysisEndpoints(service: FinancialAnalysisService):

  private val baseEndpoint = endpoint.in("api" / "v1" / "financial-analysis")

  // GET /api/v1/financial-analysis/{fiscalYear} - 財務分析を実行
  private val analyzeEndpoint: PublicEndpoint[Int, ErrorResponse, AnalysisResultDto, Any] =
    baseEndpoint.get
      .in(path[Int]("fiscalYear"))
      .out(jsonBody[AnalysisResultDto])
      .errorOut(
        oneOf[ErrorResponse](
          oneOfVariant(sttp.model.StatusCode.BadRequest, jsonBody[ErrorResponse]),
          oneOfVariant(sttp.model.StatusCode.NotFound, jsonBody[ErrorResponse])
        )
      )
      .description("指定した会計年度の財務分析を実行")

  // GET /api/v1/financial-analysis/compare?fromYear={fromYear}&toYear={toYear} - 複数期間の比較分析
  private val compareEndpoint: PublicEndpoint[(Int, Int), ErrorResponse, ComparisonResultDto, Any] =
    baseEndpoint.get
      .in("compare")
      .in(query[Int]("fromYear"))
      .in(query[Int]("toYear"))
      .out(jsonBody[ComparisonResultDto])
      .errorOut(
        oneOf[ErrorResponse](
          oneOfVariant(sttp.model.StatusCode.BadRequest, jsonBody[ErrorResponse])
        )
      )
      .description("複数期間の比較分析を実行")

  // GET /api/v1/financial-analysis/data/{fiscalYear} - 財務データを取得
  private val getDataEndpoint: PublicEndpoint[Int, ErrorResponse, FinancialDataDto, Any] =
    baseEndpoint.get
      .in("data" / path[Int]("fiscalYear"))
      .out(jsonBody[FinancialDataDto])
      .errorOut(
        oneOf[ErrorResponse](
          oneOfVariant(sttp.model.StatusCode.BadRequest, jsonBody[ErrorResponse]),
          oneOfVariant(sttp.model.StatusCode.NotFound, jsonBody[ErrorResponse])
        )
      )
      .description("指定した会計年度の財務データを取得")

  // エンドポイント定義のリスト
  val endpoints = List(
    analyzeEndpoint,
    compareEndpoint,
    getDataEndpoint
  )

  // HTTP ルートの生成
  val routes: HttpRoutes[IO] =
    val interpreter = Http4sServerInterpreter[IO]()

    val analyzeRoute = interpreter.toRoutes(
      analyzeEndpoint.serverLogic { fiscalYearInt =>
        FiscalYear(fiscalYearInt) match
          case Left(err) =>
            IO.pure(Left(ErrorResponse("ValidationError", err)))
          case Right(fiscalYear) =>
            service.analyze(fiscalYear).map {
              case Right(result) => Right(AnalysisResultDto.fromDomain(result))
              case Left(error)   => Left(ErrorResponse("AnalysisError", error))
            }
      }
    )

    val compareRoute = interpreter.toRoutes(
      compareEndpoint.serverLogic { case (fromYearInt, toYearInt) =>
        (FiscalYear(fromYearInt), FiscalYear(toYearInt)) match
          case (Left(err), _) =>
            IO.pure(Left(ErrorResponse("ValidationError", err)))
          case (_, Left(err)) =>
            IO.pure(Left(ErrorResponse("ValidationError", err)))
          case (Right(fromYear), Right(toYear)) =>
            service.compareYears(fromYear, toYear).map {
              case Right(result) => Right(ComparisonResultDto.fromDomain(result))
              case Left(error)   => Left(ErrorResponse("AnalysisError", error))
            }
      }
    )

    val getDataRoute = interpreter.toRoutes(
      getDataEndpoint.serverLogic { fiscalYearInt =>
        FiscalYear(fiscalYearInt) match
          case Left(err) =>
            IO.pure(Left(ErrorResponse("ValidationError", err)))
          case Right(fiscalYear) =>
            service.getFinancialData(fiscalYear).map {
              case Right(data) => Right(FinancialDataDto.fromDomain(data))
              case Left(error) => Left(ErrorResponse("DataError", error))
            }
      }
    )

    analyzeRoute <+> compareRoute <+> getDataRoute
