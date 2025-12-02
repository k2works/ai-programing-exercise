package com.example.accounting.infrastructure.http

import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport.*
import spray.json.*
import com.example.accounting.application.*
import com.example.accounting.application.port.in.AccountUseCase
import com.example.accounting.infrastructure.http.dto.*
import com.example.accounting.infrastructure.http.JsonFormats.*
import com.example.accounting.domain.account.AccountType

import java.net.URLDecoder
import java.nio.charset.StandardCharsets

/**
 * 勘定科目 API Routes
 */
class AccountRoutes(service: AccountUseCase):

  // 暗黙的なフォーマットのインポート
  import JsonFormats.{accountRequestFormat, accountResponseFormat, errorResponseFormat, given}

  val routes: Route =
    pathPrefix("api" / "accounts") {
      concat(
        // GET /api/accounts
        pathEnd {
          concat(
            get {
              service.getAllAccounts() match
                case Right(accounts) =>
                  val responses = accounts.map(AccountResponse.fromDomain)
                  complete(HttpEntity(ContentTypes.`application/json`, responses.toJson.prettyPrint))
                case Left(error) =>
                  complete(StatusCodes.InternalServerError -> ErrorResponse.fromAppError(error))
            },
            // POST /api/accounts
            post {
              entity(as[AccountRequest]) { request =>
                val account = AccountRequest.toDomain(request)
                service.createAccount(account) match
                  case Right(created) =>
                    complete(StatusCodes.Created -> AccountResponse.fromDomain(created))
                  case Left(error: DuplicateError) =>
                    complete(StatusCodes.Conflict -> ErrorResponse.fromAppError(error))
                  case Left(error: ValidationError) =>
                    complete(StatusCodes.BadRequest -> ErrorResponse.fromAppError(error))
                  case Left(error) =>
                    complete(StatusCodes.InternalServerError -> ErrorResponse.fromAppError(error))
              }
            },
          )
        },
        // GET /api/accounts/type/:type
        path("type" / Segment) { accountTypeStr =>
          get {
            try
              val decodedTypeStr = URLDecoder.decode(accountTypeStr, StandardCharsets.UTF_8)
              val accountType = AccountType.fromString(decodedTypeStr)
              service.getAccountsByType(accountType) match
                case Right(accounts) =>
                  val responses = accounts.map(AccountResponse.fromDomain)
                  complete(HttpEntity(ContentTypes.`application/json`, responses.toJson.prettyPrint))
                case Left(error) =>
                  complete(StatusCodes.InternalServerError -> ErrorResponse.fromAppError(error))
            catch
              case _: IllegalArgumentException =>
                complete(
                  StatusCodes.BadRequest -> ErrorResponse(
                    code = "VALIDATION_ERROR",
                    message = s"無効な勘定科目種別: $accountTypeStr",
                  )
                )
          }
        },
        // GET /api/accounts/:code, PUT /api/accounts/:code, DELETE /api/accounts/:code
        path(Segment) { code =>
          concat(
            get {
              service.getAccountByCode(code) match
                case Right(account) =>
                  complete(AccountResponse.fromDomain(account))
                case Left(error: NotFoundError) =>
                  complete(StatusCodes.NotFound -> ErrorResponse.fromAppError(error))
                case Left(error) =>
                  complete(StatusCodes.InternalServerError -> ErrorResponse.fromAppError(error))
            },
            put {
              entity(as[AccountRequest]) { request =>
                val account = AccountRequest.toDomain(request)
                service.updateAccount(code, account) match
                  case Right(updated) =>
                    complete(AccountResponse.fromDomain(updated))
                  case Left(error: NotFoundError) =>
                    complete(StatusCodes.NotFound -> ErrorResponse.fromAppError(error))
                  case Left(error: ValidationError) =>
                    complete(StatusCodes.BadRequest -> ErrorResponse.fromAppError(error))
                  case Left(error) =>
                    complete(StatusCodes.InternalServerError -> ErrorResponse.fromAppError(error))
              }
            },
            delete {
              service.deleteAccount(code) match
                case Right(_) =>
                  complete(StatusCodes.NoContent)
                case Left(error: NotFoundError) =>
                  complete(StatusCodes.NotFound -> ErrorResponse.fromAppError(error))
                case Left(error) =>
                  complete(StatusCodes.InternalServerError -> ErrorResponse.fromAppError(error))
            },
          )
        },
      )
    }
