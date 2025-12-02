package com.example.financial.adapter.api

import cats.effect.IO
import cats.implicits.*
import org.http4s.HttpRoutes
import sttp.tapir.*
import sttp.tapir.server.http4s.Http4sServerInterpreter
import sttp.tapir.json.circe.*
import sttp.tapir.generic.auto.*
import io.circe.generic.auto.*
import com.example.financial.application.{JournalService, JournalEntryRequest}
import com.example.financial.domain.{Journal, JournalEntry}
import com.example.common.domain.*
import com.example.common.json.JsonCodecs.given
import java.time.{LocalDate, LocalDateTime}

/**
 * 仕訳 API DTO
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

object JournalDto:
  def fromDomain(journal: Journal): JournalDto =
    JournalDto(
      journalId = journal.journalId,
      journalDate = journal.journalDate,
      description = journal.description,
      fiscalYear = journal.fiscalYear.value,
      entries = journal.entries.map(JournalEntryDto.fromDomain),
      createdAt = journal.createdAt,
      updatedAt = journal.updatedAt
    )

object JournalEntryDto:
  def fromDomain(entry: JournalEntry): JournalEntryDto =
    JournalEntryDto(
      entryId = entry.entryId,
      accountCode = entry.accountCode.value,
      debitAmount = entry.debitAmount.value,
      creditAmount = entry.creditAmount.value,
      description = entry.description
    )

case class CreateJournalRequest(
    journalDate: LocalDate,
    description: String,
    fiscalYear: Int,
    entries: List[CreateJournalEntryRequest]
)

case class CreateJournalEntryRequest(
    accountCode: String,
    debitAmount: BigDecimal,
    creditAmount: BigDecimal,
    description: String = ""
)

/**
 * 仕訳 API エンドポイント
 */
class JournalEndpoints(service: JournalService):

  private val baseEndpoint = endpoint.in("api" / "v1" / "journals")

  // GET /api/v1/journals?fiscalYear={year} - 仕訳一覧を取得
  private val listJournalsEndpoint: PublicEndpoint[Int, ErrorResponse, List[JournalDto], Any] =
    baseEndpoint.get
      .in(query[Int]("fiscalYear"))
      .out(jsonBody[List[JournalDto]])
      .errorOut(
        oneOf[ErrorResponse](
          oneOfVariant(sttp.model.StatusCode.BadRequest, jsonBody[ErrorResponse])
        )
      )
      .description("指定した会計年度の仕訳一覧を取得")

  // GET /api/v1/journals/{id} - 仕訳を取得
  private val getJournalEndpoint: PublicEndpoint[Long, ErrorResponse, JournalDto, Any] =
    baseEndpoint.get
      .in(path[Long]("journalId"))
      .out(jsonBody[JournalDto])
      .errorOut(
        oneOf[ErrorResponse](
          oneOfVariant(sttp.model.StatusCode.NotFound, jsonBody[ErrorResponse])
        )
      )
      .description("仕訳IDで取得")

  // POST /api/v1/journals - 仕訳を作成
  private val createJournalEndpoint: PublicEndpoint[CreateJournalRequest, ErrorResponse, JournalDto, Any] =
    baseEndpoint.post
      .in(jsonBody[CreateJournalRequest])
      .out(jsonBody[JournalDto])
      .errorOut(
        oneOf[ErrorResponse](
          oneOfVariant(sttp.model.StatusCode.BadRequest, jsonBody[ErrorResponse])
        )
      )
      .description("仕訳を作成")

  // DELETE /api/v1/journals/{id} - 仕訳を削除
  private val deleteJournalEndpoint: PublicEndpoint[Long, ErrorResponse, Unit, Any] =
    baseEndpoint.delete
      .in(path[Long]("journalId"))
      .out(emptyOutput)
      .errorOut(
        oneOf[ErrorResponse](
          oneOfVariant(sttp.model.StatusCode.NotFound, jsonBody[ErrorResponse])
        )
      )
      .description("仕訳を削除")

  // エンドポイント定義のリスト
  val endpoints = List(
    listJournalsEndpoint,
    getJournalEndpoint,
    createJournalEndpoint,
    deleteJournalEndpoint
  )

  // HTTP ルートの生成
  val routes: HttpRoutes[IO] =
    val interpreter = Http4sServerInterpreter[IO]()

    val listRoute = interpreter.toRoutes(
      listJournalsEndpoint.serverLogic { fiscalYearInt =>
        FiscalYear(fiscalYearInt) match
          case Left(err) =>
            IO.pure(Left(ErrorResponse("ValidationError", err)))
          case Right(fiscalYear) =>
            service.findAll(fiscalYear).map { journals =>
              Right(journals.map(JournalDto.fromDomain))
            }
      }
    )

    val getRoute = interpreter.toRoutes(
      getJournalEndpoint.serverLogic { journalId =>
        service.findById(journalId).map {
          case Right(journal) => Right(JournalDto.fromDomain(journal))
          case Left(error)    => Left(ErrorResponse(error.getClass.getSimpleName, error.message))
        }
      }
    )

    val createRoute = interpreter.toRoutes(
      createJournalEndpoint.serverLogic { req =>
        // リクエストの変換
        val entriesResult = req.entries.traverse { entry =>
          AccountCode(entry.accountCode).map { code =>
            JournalEntryRequest(
              accountCode = code,
              debitAmount = Money(entry.debitAmount),
              creditAmount = Money(entry.creditAmount),
              description = entry.description
            )
          }
        }

        val fiscalYearResult = FiscalYear(req.fiscalYear)

        (entriesResult, fiscalYearResult) match
          case (Left(err), _) =>
            IO.pure(Left(ErrorResponse("ValidationError", err)))
          case (_, Left(err)) =>
            IO.pure(Left(ErrorResponse("ValidationError", err)))
          case (Right(entries), Right(fiscalYear)) =>
            service
              .create(
                journalDate = req.journalDate,
                description = req.description,
                fiscalYear = fiscalYear,
                entries = entries
              )
              .map {
                case Right(journal) => Right(JournalDto.fromDomain(journal))
                case Left(error)    => Left(ErrorResponse(error.getClass.getSimpleName, error.message))
              }
      }
    )

    val deleteRoute = interpreter.toRoutes(
      deleteJournalEndpoint.serverLogic { journalId =>
        service.delete(journalId).map {
          case Right(_)    => Right(())
          case Left(error) => Left(ErrorResponse(error.getClass.getSimpleName, error.message))
        }
      }
    )

    listRoute <+> getRoute <+> createRoute <+> deleteRoute
