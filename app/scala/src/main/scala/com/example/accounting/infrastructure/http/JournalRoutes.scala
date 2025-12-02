package com.example.accounting.infrastructure.http

import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport.*
import spray.json.*
import com.example.accounting.application.*
import com.example.accounting.infrastructure.http.dto.*
import com.example.accounting.infrastructure.http.JsonFormats.*

import java.time.LocalDate
import java.time.format.DateTimeParseException

/**
 * 仕訳 API Routes
 */
class JournalRoutes(service: JournalService):

  // 暗黙的なフォーマットのインポート
  import JsonFormats.{
    journalRequestFormat,
    journalResponseFormat,
    listJournalResponseFormat,
    journalValidationResponseFormat,
    errorResponseFormat,
    given,
  }

  val routes: Route =
    pathPrefix("api" / "journals") {
      concat(
        // GET /api/journals?from=...&to=...
        pathEnd {
          concat(
            get {
              parameters("from".as[String], "to".as[String]) { (fromStr, toStr) =>
                try
                  val from = LocalDate.parse(fromStr)
                  val to = LocalDate.parse(toStr)
                  service.getJournalsByDateRange(from, to) match
                    case Right(journals) =>
                      val responses = journals.map { case (journal, details, dcDetails) =>
                        JournalResponse.fromDomain(journal, details, dcDetails)
                      }
                      val jsonArray = JsArray(responses.map(_.toJson).toVector)
                      complete(HttpEntity(ContentTypes.`application/json`, jsonArray.prettyPrint))
                    case Left(error) =>
                      complete(StatusCodes.InternalServerError -> ErrorResponse.fromAppError(error))
                catch
                  case _: DateTimeParseException =>
                    complete(
                      StatusCodes.BadRequest -> ErrorResponse(
                        code = "VALIDATION_ERROR",
                        message = "日付の形式が不正です（YYYY-MM-DD）",
                      )
                    )
              }
            },
            // POST /api/journals
            post {
              entity(as[JournalRequest]) { request =>
                val (journal, details, dcDetails) = JournalRequest.toDomain(request)
                service.createJournal(journal, details, dcDetails) match
                  case Right((created, createdDetails, createdDCDetails)) =>
                    complete(
                      StatusCodes.Created -> JournalResponse.fromDomain(created, createdDetails, createdDCDetails)
                    )
                  case Left(error: DuplicateError) =>
                    complete(StatusCodes.Conflict -> ErrorResponse.fromAppError(error))
                  case Left(error: ValidationError) =>
                    complete(StatusCodes.BadRequest -> ErrorResponse.fromAppError(error))
                  case Left(error: InvalidJournalError) =>
                    complete(StatusCodes.BadRequest -> ErrorResponse.fromAppError(error))
                  case Left(error) =>
                    complete(StatusCodes.InternalServerError -> ErrorResponse.fromAppError(error))
              }
            },
          )
        },
        // GET /api/journals/:journalNo/validate
        path(Segment / "validate") { journalNo =>
          get {
            service.validateBalance(journalNo) match
              case Right((isBalanced, totalDebit, totalCredit)) =>
                complete(
                  JournalValidationResponse(
                    journalNo = journalNo,
                    isBalanced = isBalanced,
                    totalDebit = totalDebit,
                    totalCredit = totalCredit,
                  )
                )
              case Left(error: NotFoundError) =>
                complete(StatusCodes.NotFound -> ErrorResponse.fromAppError(error))
              case Left(error) =>
                complete(StatusCodes.InternalServerError -> ErrorResponse.fromAppError(error))
          }
        },
        // GET /api/journals/:journalNo, DELETE /api/journals/:journalNo
        path(Segment) { journalNo =>
          concat(
            get {
              service.getJournal(journalNo) match
                case Right((journal, details, dcDetails)) =>
                  complete(JournalResponse.fromDomain(journal, details, dcDetails))
                case Left(error: NotFoundError) =>
                  complete(StatusCodes.NotFound -> ErrorResponse.fromAppError(error))
                case Left(error) =>
                  complete(StatusCodes.InternalServerError -> ErrorResponse.fromAppError(error))
            },
            delete {
              service.deleteJournal(journalNo) match
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
