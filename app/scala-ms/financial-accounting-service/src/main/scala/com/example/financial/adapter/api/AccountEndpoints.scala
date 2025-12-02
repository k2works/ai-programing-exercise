package com.example.financial.adapter.api

import cats.effect.IO
import cats.implicits.*
import org.http4s.HttpRoutes
import sttp.tapir.*
import sttp.tapir.server.http4s.Http4sServerInterpreter
import sttp.tapir.json.circe.*
import sttp.tapir.generic.auto.*
import io.circe.generic.auto.*
import com.example.financial.application.AccountService
import com.example.financial.domain.Account
import com.example.common.domain.*
import com.example.common.json.JsonCodecs.given
import java.time.LocalDateTime

/**
 * 勘定科目 API DTO
 */
case class AccountDto(
    accountId: Option[Long],
    accountCode: String,
    accountName: String,
    accountType: String,
    isSummaryAccount: Boolean,
    displayOrder: Int,
    isAggregationTarget: Boolean,
    balance: BigDecimal,
    createdAt: LocalDateTime,
    updatedAt: LocalDateTime
)

object AccountDto:
  def fromDomain(account: Account): AccountDto =
    AccountDto(
      accountId = account.accountId,
      accountCode = account.accountCode.value,
      accountName = account.accountName,
      accountType = account.accountType.toString.toLowerCase,
      isSummaryAccount = account.isSummaryAccount,
      displayOrder = account.displayOrder,
      isAggregationTarget = account.isAggregationTarget,
      balance = account.balance.value,
      createdAt = account.createdAt,
      updatedAt = account.updatedAt
    )

case class CreateAccountRequest(
    accountCode: String,
    accountName: String,
    accountType: String,
    isSummaryAccount: Boolean = false,
    displayOrder: Int = 0,
    isAggregationTarget: Boolean = true
)

case class UpdateAccountRequest(
    accountName: Option[String] = None,
    displayOrder: Option[Int] = None,
    isAggregationTarget: Option[Boolean] = None
)

case class ErrorResponse(
    errorType: String,
    message: String
)

/**
 * 勘定科目 API エンドポイント
 */
class AccountEndpoints(service: AccountService):

  private val baseEndpoint = endpoint.in("api" / "v1" / "accounts")

  // GET /api/v1/accounts - 勘定科目一覧を取得
  private val listAccountsEndpoint: PublicEndpoint[Unit, Unit, List[AccountDto], Any] =
    baseEndpoint.get
      .out(jsonBody[List[AccountDto]])
      .description("勘定科目一覧を取得")

  // GET /api/v1/accounts/{code} - 勘定科目を取得
  private val getAccountEndpoint: PublicEndpoint[String, ErrorResponse, AccountDto, Any] =
    baseEndpoint.get
      .in(path[String]("accountCode"))
      .out(jsonBody[AccountDto])
      .errorOut(
        oneOf[ErrorResponse](
          oneOfVariant(sttp.model.StatusCode.NotFound, jsonBody[ErrorResponse])
        )
      )
      .description("勘定科目コードで取得")

  // POST /api/v1/accounts - 勘定科目を作成
  private val createAccountEndpoint: PublicEndpoint[CreateAccountRequest, ErrorResponse, AccountDto, Any] =
    baseEndpoint.post
      .in(jsonBody[CreateAccountRequest])
      .out(jsonBody[AccountDto])
      .errorOut(
        oneOf[ErrorResponse](
          oneOfVariant(sttp.model.StatusCode.BadRequest, jsonBody[ErrorResponse]),
          oneOfVariant(sttp.model.StatusCode.Conflict, jsonBody[ErrorResponse])
        )
      )
      .description("勘定科目を作成")

  // PUT /api/v1/accounts/{code} - 勘定科目を更新
  private val updateAccountEndpoint: PublicEndpoint[(String, UpdateAccountRequest), ErrorResponse, AccountDto, Any] =
    baseEndpoint.put
      .in(path[String]("accountCode"))
      .in(jsonBody[UpdateAccountRequest])
      .out(jsonBody[AccountDto])
      .errorOut(
        oneOf[ErrorResponse](
          oneOfVariant(sttp.model.StatusCode.NotFound, jsonBody[ErrorResponse])
        )
      )
      .description("勘定科目を更新")

  // DELETE /api/v1/accounts/{code} - 勘定科目を削除
  private val deleteAccountEndpoint: PublicEndpoint[String, ErrorResponse, Unit, Any] =
    baseEndpoint.delete
      .in(path[String]("accountCode"))
      .out(emptyOutput)
      .errorOut(
        oneOf[ErrorResponse](
          oneOfVariant(sttp.model.StatusCode.NotFound, jsonBody[ErrorResponse])
        )
      )
      .description("勘定科目を削除")

  // エンドポイント定義のリスト（OpenAPI 生成用）
  val endpoints = List(
    listAccountsEndpoint,
    getAccountEndpoint,
    createAccountEndpoint,
    updateAccountEndpoint,
    deleteAccountEndpoint
  )

  // HTTP ルートの生成
  val routes: HttpRoutes[IO] =
    val interpreter = Http4sServerInterpreter[IO]()

    val listRoute = interpreter.toRoutes(
      listAccountsEndpoint.serverLogic { _ =>
        service.findAll().map(accounts => Right(accounts.map(AccountDto.fromDomain)))
      }
    )

    val getRoute = interpreter.toRoutes(
      getAccountEndpoint.serverLogic { code =>
        AccountCode(code) match
          case Left(err) =>
            IO.pure(Left(ErrorResponse("ValidationError", err)))
          case Right(accountCode) =>
            service.findByCode(accountCode).map {
              case Right(account) => Right(AccountDto.fromDomain(account))
              case Left(error)    => Left(ErrorResponse(error.getClass.getSimpleName, error.message))
            }
      }
    )

    val createRoute = interpreter.toRoutes(
      createAccountEndpoint.serverLogic { req =>
        (for
          accountCode <- AccountCode(req.accountCode)
          accountType <- AccountType.fromString(req.accountType)
        yield (accountCode, accountType)) match
          case Left(err) =>
            IO.pure(Left(ErrorResponse("ValidationError", err)))
          case Right((accountCode, accountType)) =>
            service
              .create(
                accountCode = accountCode,
                accountName = req.accountName,
                accountType = accountType,
                isSummaryAccount = req.isSummaryAccount,
                displayOrder = req.displayOrder,
                isAggregationTarget = req.isAggregationTarget
              )
              .map {
                case Right(account) => Right(AccountDto.fromDomain(account))
                case Left(error)    => Left(ErrorResponse(error.getClass.getSimpleName, error.message))
              }
      }
    )

    val updateRoute = interpreter.toRoutes(
      updateAccountEndpoint.serverLogic { case (code, req) =>
        AccountCode(code) match
          case Left(err) =>
            IO.pure(Left(ErrorResponse("ValidationError", err)))
          case Right(accountCode) =>
            service
              .update(
                accountCode = accountCode,
                accountName = req.accountName,
                displayOrder = req.displayOrder,
                isAggregationTarget = req.isAggregationTarget
              )
              .map {
                case Right(account) => Right(AccountDto.fromDomain(account))
                case Left(error)    => Left(ErrorResponse(error.getClass.getSimpleName, error.message))
              }
      }
    )

    val deleteRoute = interpreter.toRoutes(
      deleteAccountEndpoint.serverLogic { code =>
        AccountCode(code) match
          case Left(err) =>
            IO.pure(Left(ErrorResponse("ValidationError", err)))
          case Right(accountCode) =>
            service.delete(accountCode).map {
              case Right(_)    => Right(())
              case Left(error) => Left(ErrorResponse(error.getClass.getSimpleName, error.message))
            }
      }
    )

    listRoute <+> getRoute <+> createRoute <+> updateRoute <+> deleteRoute
