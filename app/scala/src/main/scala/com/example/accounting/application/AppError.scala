package com.example.accounting.application

/**
 * アプリケーションエラーの基底 trait
 */
sealed trait AppError:
  def message: String

/**
 * リソースが見つからないエラー
 */
case class NotFoundError(message: String) extends AppError

/**
 * バリデーションエラー
 */
case class ValidationError(message: String) extends AppError

/**
 * 重複エラー
 */
case class DuplicateError(message: String) extends AppError

/**
 * 仕訳エラー（貸借不一致など）
 */
case class InvalidJournalError(message: String) extends AppError

/**
 * データベースエラー
 */
case class DatabaseError(message: String, cause: Option[Throwable] = None) extends AppError

/**
 * 内部エラー
 */
case class InternalError(message: String, cause: Option[Throwable] = None) extends AppError
