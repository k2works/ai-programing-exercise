package com.example.accounting.infrastructure.in.http

import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport.*
import spray.json.*
import com.example.accounting.application.*
import com.example.accounting.application.port.in.AuditLogUseCase
import com.example.accounting.infrastructure.in.http.dto.*
import com.example.accounting.infrastructure.in.http.JsonFormats.*

import java.time.Instant

/**
 * 監査ログ API Routes
 */
class AuditLogRoutes(service: AuditLogUseCase):

  import JsonFormats.{auditLogResponseFormat, listAuditLogResponseFormat, errorResponseFormat, given}

  val routes: Route =
    pathPrefix("api" / "audit-logs") {
      concat(
        // GET /api/audit-logs?startDate=...&endDate=...&limit=...
        pathEnd {
          get {
            parameters(
              "startDate".optional,
              "endDate".optional,
              "limit".as[Int].withDefault(100),
            ) { (startDateStr, endDateStr, limit) =>
              try
                val startDate = startDateStr.map(Instant.parse).getOrElse(Instant.now().minusSeconds(86400 * 30))
                val endDate = endDateStr.map(Instant.parse).getOrElse(Instant.now())

                service.getAuditLogsByPeriod(startDate, endDate, limit) match
                  case Right(logs) =>
                    val responses = logs.map(AuditLogResponse.fromDomain)
                    complete(HttpEntity(ContentTypes.`application/json`, responses.toJson.prettyPrint))
                  case Left(error) =>
                    complete(StatusCodes.InternalServerError -> ErrorResponse.fromAppError(error))
              catch
                case e: Exception =>
                  complete(
                    StatusCodes.BadRequest -> ErrorResponse(
                      code = "VALIDATION_ERROR",
                      message = s"無効な日付形式: ${e.getMessage}",
                    )
                  )
            }
          }
        },
        // GET /api/audit-logs/entity/:entityType/:entityId
        path("entity" / Segment / Segment) { (entityType, entityId) =>
          get {
            service.getAuditLogsByEntity(entityType, entityId) match
              case Right(logs) =>
                val responses = logs.map(AuditLogResponse.fromDomain)
                complete(HttpEntity(ContentTypes.`application/json`, responses.toJson.prettyPrint))
              case Left(error) =>
                complete(StatusCodes.InternalServerError -> ErrorResponse.fromAppError(error))
          }
        },
        // GET /api/audit-logs/user/:userId
        path("user" / Segment) { userId =>
          get {
            parameters(
              "startDate".optional,
              "endDate".optional,
            ) { (startDateStr, endDateStr) =>
              try
                val startDate = startDateStr.map(Instant.parse).getOrElse(Instant.now().minusSeconds(86400 * 30))
                val endDate = endDateStr.map(Instant.parse).getOrElse(Instant.now())

                service.getAuditLogsByUser(userId, startDate, endDate) match
                  case Right(logs) =>
                    val responses = logs.map(AuditLogResponse.fromDomain)
                    complete(HttpEntity(ContentTypes.`application/json`, responses.toJson.prettyPrint))
                  case Left(error) =>
                    complete(StatusCodes.InternalServerError -> ErrorResponse.fromAppError(error))
              catch
                case e: Exception =>
                  complete(
                    StatusCodes.BadRequest -> ErrorResponse(
                      code = "VALIDATION_ERROR",
                      message = s"無効な日付形式: ${e.getMessage}",
                    )
                  )
            }
          }
        },
        // GET /api/audit-logs/action/:action
        path("action" / Segment) { action =>
          get {
            parameters(
              "startDate".optional,
              "endDate".optional,
            ) { (startDateStr, endDateStr) =>
              try
                val startDate = startDateStr.map(Instant.parse).getOrElse(Instant.now().minusSeconds(86400 * 30))
                val endDate = endDateStr.map(Instant.parse).getOrElse(Instant.now())

                service.getAuditLogsByAction(action, startDate, endDate) match
                  case Right(logs) =>
                    val responses = logs.map(AuditLogResponse.fromDomain)
                    complete(HttpEntity(ContentTypes.`application/json`, responses.toJson.prettyPrint))
                  case Left(error) =>
                    complete(StatusCodes.InternalServerError -> ErrorResponse.fromAppError(error))
              catch
                case e: Exception =>
                  complete(
                    StatusCodes.BadRequest -> ErrorResponse(
                      code = "VALIDATION_ERROR",
                      message = s"無効な日付形式: ${e.getMessage}",
                    )
                  )
            }
          }
        },
      )
    }
