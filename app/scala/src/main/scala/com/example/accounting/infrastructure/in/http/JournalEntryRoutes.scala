package com.example.accounting.infrastructure.in.http

import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport.*
import spray.json.*
import com.example.accounting.application.*
import com.example.accounting.application.port.in.*
import com.example.accounting.infrastructure.in.http.dto.*
import com.example.accounting.infrastructure.in.http.JsonFormats.*

import java.time.Instant
import java.time.format.DateTimeParseException

/**
 * 仕訳エントリ API Routes（イベントソーシング）
 */
class JournalEntryRoutes(service: JournalEntryUseCase):

  import JsonFormats.{
    journalEntryCreateRequestFormat,
    journalEntryApproveRequestFormat,
    journalEntryRejectRequestFormat,
    journalEntryDeleteRequestFormat,
    journalEntryResponseFormat,
    journalEntryEventResponseFormat,
    listJournalEntryEventResponseFormat,
    errorResponseFormat,
    given,
  }

  val routes: Route =
    pathPrefix("api" / "journal-entries") {
      concat(
        // POST /api/journal-entries - 仕訳作成
        pathEnd {
          post {
            entity(as[JournalEntryCreateRequest]) { request =>
              val command = JournalEntryCreateRequest.toCommand(request)
              service.createJournalEntry(command) match
                case Right(aggregate) =>
                  complete(StatusCodes.Created -> JournalEntryResponse.fromAggregate(aggregate))
                case Left(error: ValidationError) =>
                  complete(StatusCodes.BadRequest -> ErrorResponse.fromAppError(error))
                case Left(error: InvalidJournalError) =>
                  complete(StatusCodes.BadRequest -> ErrorResponse.fromAppError(error))
                case Left(error) =>
                  complete(StatusCodes.InternalServerError -> ErrorResponse.fromAppError(error))
            }
          }
        },
        // GET /api/journal-entries/:id/history - イベント履歴取得
        path(Segment / "history") { aggregateId =>
          get {
            service.getJournalEntryHistory(aggregateId) match
              case Right(events) =>
                val responses = events.map(JournalEntryEventResponse.fromEvent)
                complete(responses)
              case Left(error: NotFoundError) =>
                complete(StatusCodes.NotFound -> ErrorResponse.fromAppError(error))
              case Left(error) =>
                complete(StatusCodes.InternalServerError -> ErrorResponse.fromAppError(error))
          }
        },
        // GET /api/journal-entries/:id/at?asOf=... - 特定時点の状態取得
        path(Segment / "at") { aggregateId =>
          get {
            parameter("asOf".as[String]) { asOfStr =>
              try
                val asOf = Instant.parse(asOfStr)
                service.getJournalEntryAt(aggregateId, asOf) match
                  case Right(aggregate) =>
                    complete(JournalEntryResponse.fromAggregate(aggregate))
                  case Left(error: NotFoundError) =>
                    complete(StatusCodes.NotFound -> ErrorResponse.fromAppError(error))
                  case Left(error) =>
                    complete(StatusCodes.InternalServerError -> ErrorResponse.fromAppError(error))
              catch
                case _: DateTimeParseException =>
                  complete(
                    StatusCodes.BadRequest -> ErrorResponse(
                      code = "VALIDATION_ERROR",
                      message = "日時の形式が不正です（ISO-8601）",
                    )
                  )
            }
          }
        },
        // POST /api/journal-entries/:id/approve - 承認
        path(Segment / "approve") { aggregateId =>
          post {
            entity(as[JournalEntryApproveRequest]) { request =>
              val command = ApproveJournalEntryCommand(aggregateId, request.approvedBy)
              service.approveJournalEntry(command) match
                case Right(aggregate) =>
                  complete(JournalEntryResponse.fromAggregate(aggregate))
                case Left(error: NotFoundError) =>
                  complete(StatusCodes.NotFound -> ErrorResponse.fromAppError(error))
                case Left(error: ValidationError) =>
                  complete(StatusCodes.BadRequest -> ErrorResponse.fromAppError(error))
                case Left(error) =>
                  complete(StatusCodes.InternalServerError -> ErrorResponse.fromAppError(error))
            }
          }
        },
        // POST /api/journal-entries/:id/reject - 却下
        path(Segment / "reject") { aggregateId =>
          post {
            entity(as[JournalEntryRejectRequest]) { request =>
              val command = RejectJournalEntryCommand(aggregateId, request.rejectedBy, request.reason)
              service.rejectJournalEntry(command) match
                case Right(aggregate) =>
                  complete(JournalEntryResponse.fromAggregate(aggregate))
                case Left(error: NotFoundError) =>
                  complete(StatusCodes.NotFound -> ErrorResponse.fromAppError(error))
                case Left(error: ValidationError) =>
                  complete(StatusCodes.BadRequest -> ErrorResponse.fromAppError(error))
                case Left(error) =>
                  complete(StatusCodes.InternalServerError -> ErrorResponse.fromAppError(error))
            }
          }
        },
        // GET /api/journal-entries/:id, DELETE /api/journal-entries/:id
        path(Segment) { aggregateId =>
          concat(
            get {
              service.getJournalEntry(aggregateId) match
                case Right(aggregate) =>
                  complete(JournalEntryResponse.fromAggregate(aggregate))
                case Left(error: NotFoundError) =>
                  complete(StatusCodes.NotFound -> ErrorResponse.fromAppError(error))
                case Left(error) =>
                  complete(StatusCodes.InternalServerError -> ErrorResponse.fromAppError(error))
            },
            delete {
              entity(as[JournalEntryDeleteRequest]) { request =>
                val command = DeleteJournalEntryCommand(aggregateId, request.deletedBy, request.reason)
                service.deleteJournalEntry(command) match
                  case Right(aggregate) =>
                    complete(JournalEntryResponse.fromAggregate(aggregate))
                  case Left(error: NotFoundError) =>
                    complete(StatusCodes.NotFound -> ErrorResponse.fromAppError(error))
                  case Left(error: ValidationError) =>
                    complete(StatusCodes.BadRequest -> ErrorResponse.fromAppError(error))
                  case Left(error) =>
                    complete(StatusCodes.InternalServerError -> ErrorResponse.fromAppError(error))
              }
            },
          )
        },
      )
    }
