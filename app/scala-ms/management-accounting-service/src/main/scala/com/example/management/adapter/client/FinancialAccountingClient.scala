package com.example.management.adapter.client

import cats.effect.IO
import cats.implicits.*
import org.http4s.client.Client
import org.http4s.circe.CirceEntityDecoder.*
import org.http4s.{Uri, Request, Method}
import io.circe.generic.auto.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import com.example.management.domain.FinancialData
import com.example.common.domain.*
import java.time.{LocalDate, LocalDateTime}

/**
 * 財務会計サービスから取得するデータの DTO
 * （財務会計コンテキストの形式）
 */
case class JournalDto(
    journalId: Option[Long],
    journalDate: LocalDate,
    description: String,
    fiscalYear: Int,
    entries: List[JournalEntryDto],
    createdAt: LocalDateTime,
    updatedAt: LocalDateTime
)

case class JournalEntryDto(
    entryId: Option[Long],
    accountCode: String,
    debitAmount: BigDecimal,
    creditAmount: BigDecimal,
    description: String
)

case class AccountBalanceDto(
    accountCode: String,
    fiscalYear: Int,
    debitTotal: BigDecimal,
    creditTotal: BigDecimal,
    balance: BigDecimal
)

/**
 * 財務会計サービスクライアント（腐敗防止層）
 *
 * 財務会計コンテキストからデータを取得し、
 * 管理会計コンテキストの形式に変換します。
 * これにより、財務会計コンテキストのモデル変更が
 * 管理会計コンテキストに影響しないようにします。
 */
class FinancialAccountingClient(
    httpClient: Client[IO],
    baseUrl: String
):
  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  /**
   * 指定した会計年度の財務データを取得
   *
   * 財務会計サービスから仕訳データを取得し、
   * 管理会計コンテキストの FinancialData に変換
   */
  def getFinancialData(fiscalYear: FiscalYear): IO[Either[String, FinancialData]] =
    for
      _ <- logger.info(s"Fetching financial data for fiscal year ${fiscalYear.value}")
      journalsResult <- fetchJournals(fiscalYear)
      result <- journalsResult match
        case Right(journals) =>
          IO.pure(Right(convertToFinancialData(fiscalYear, journals)))
        case Left(error) =>
          IO.pure(Left(error))
    yield result

  /**
   * 複数年度の財務データを取得
   */
  def getFinancialDataRange(
      fromYear: FiscalYear,
      toYear: FiscalYear
  ): IO[Either[String, List[FinancialData]]] =
    val years = (fromYear.value to toYear.value).toList
    years
      .traverse { year =>
        FiscalYear(year) match
          case Right(fy) => getFinancialData(fy)
          case Left(err) => IO.pure(Left(err))
      }
      .map { results =>
        val errors = results.collect { case Left(e) => e }
        val data = results.collect { case Right(d) => d }
        if errors.nonEmpty then Left(errors.mkString(", "))
        else Right(data)
      }

  /**
   * 財務会計サービスから仕訳データを取得
   */
  private def fetchJournals(fiscalYear: FiscalYear): IO[Either[String, List[JournalDto]]] =
    val uri = Uri
      .unsafeFromString(s"$baseUrl/api/v1/journals")
      .withQueryParam("fiscalYear", fiscalYear.value)

    httpClient
      .expect[List[JournalDto]](uri)
      .map(Right(_))
      .handleErrorWith { error =>
        logger.error(error)(s"Failed to fetch journals: ${error.getMessage}") >>
          IO.pure(Left(s"Failed to fetch journals: ${error.getMessage}"))
      }

  /**
   * 仕訳データを財務データに変換（ACL の中核ロジック）
   *
   * 財務会計コンテキストの仕訳を管理会計コンテキストの
   * FinancialData に変換します。この変換ロジックにより、
   * 財務会計コンテキストのモデル変更の影響を局所化します。
   */
  private def convertToFinancialData(
      fiscalYear: FiscalYear,
      journals: List[JournalDto]
  ): FinancialData =
    // 勘定科目コードのプレフィックスで分類して集計
    val entries = journals.flatMap(_.entries)

    // 科目コードプレフィックスによる集計
    def sumByPrefix(prefix: String): BigDecimal =
      entries
        .filter(_.accountCode.startsWith(prefix))
        .map(e => e.creditAmount - e.debitAmount) // 収益・負債・資本は貸方が正
        .sum

    def sumDebitByPrefix(prefix: String): BigDecimal =
      entries
        .filter(_.accountCode.startsWith(prefix))
        .map(e => e.debitAmount - e.creditAmount) // 資産・費用は借方が正
        .sum

    // 損益計算書項目（勘定科目コードの体系に依存）
    val sales = sumByPrefix("41")              // 売上高
    val costOfSales = sumDebitByPrefix("51")   // 売上原価
    val sellingExpenses = sumDebitByPrefix("52") // 販管費
    val nonOperatingIncome = sumByPrefix("42") // 営業外収益
    val nonOperatingExpenses = sumDebitByPrefix("53") // 営業外費用

    // 貸借対照表項目
    val currentAssets = sumDebitByPrefix("11")  // 流動資産
    val fixedAssets = sumDebitByPrefix("12")    // 固定資産
    val currentLiabilities = sumByPrefix("21")  // 流動負債
    val fixedLiabilities = sumByPrefix("22")    // 固定負債
    val equity = sumByPrefix("3")               // 純資産

    // 計算項目
    val grossProfit = sales - costOfSales
    val operatingProfit = grossProfit - sellingExpenses
    val ordinaryProfit = operatingProfit + nonOperatingIncome - nonOperatingExpenses
    val totalAssets = currentAssets + fixedAssets
    val totalLiabilities = currentLiabilities + fixedLiabilities

    FinancialData(
      fiscalYear = fiscalYear,
      sales = Money(sales),
      costOfSales = Money(costOfSales),
      grossProfit = Money(grossProfit),
      sellingGeneralExpenses = Money(sellingExpenses),
      operatingProfit = Money(operatingProfit),
      nonOperatingIncome = Money(nonOperatingIncome),
      nonOperatingExpenses = Money(nonOperatingExpenses),
      ordinaryProfit = Money(ordinaryProfit),
      currentAssets = Money(currentAssets),
      fixedAssets = Money(fixedAssets),
      totalAssets = Money(totalAssets),
      currentLiabilities = Money(currentLiabilities),
      fixedLiabilities = Money(fixedLiabilities),
      totalLiabilities = Money(totalLiabilities),
      equity = Money(equity)
    )
