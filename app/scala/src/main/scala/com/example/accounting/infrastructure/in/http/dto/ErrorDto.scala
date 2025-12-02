package com.example.accounting.infrastructure.in.http.dto

import com.example.accounting.application.*

/**
 * エラーレスポンス DTO
 */
case class ErrorResponse(
    code: String,
    message: String,
    details: Option[List[String]] = None,
)

object ErrorResponse:
  def fromAppError(error: AppError): ErrorResponse = error match
    case NotFoundError(msg) =>
      ErrorResponse(code = "NOT_FOUND", message = msg)
    case ValidationError(msg) =>
      ErrorResponse(code = "VALIDATION_ERROR", message = msg)
    case DuplicateError(msg) =>
      ErrorResponse(code = "DUPLICATE_ERROR", message = msg)
    case InvalidJournalError(msg) =>
      ErrorResponse(code = "INVALID_JOURNAL", message = msg)
    case DatabaseError(msg, cause) =>
      ErrorResponse(
        code = "DATABASE_ERROR",
        message = "データベースエラーが発生しました",
        details = cause.map(e => List(e.getMessage)),
      )
    case InternalError(msg, cause) =>
      ErrorResponse(
        code = "INTERNAL_ERROR",
        message = "内部エラーが発生しました",
        details = cause.map(e => List(e.getMessage)),
      )
