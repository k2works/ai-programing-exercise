package com.example.accounting.domain.journal

import java.time.LocalDateTime

/**
 * 自動仕訳実行結果
 */
enum ExecutionResult(val code: String):
  case Running extends ExecutionResult("RUNNING")
  case Success extends ExecutionResult("SUCCESS")
  case Failed extends ExecutionResult("FAILED")
  case Partial extends ExecutionResult("PARTIAL")

object ExecutionResult:
  def fromCode(code: String): ExecutionResult =
    code match
      case "RUNNING" => Running
      case "SUCCESS" => Success
      case "FAILED"  => Failed
      case "PARTIAL" => Partial
      case _         => throw new IllegalArgumentException(s"Invalid execution result: $code")

/**
 * 自動仕訳管理
 */
case class AutoJournalManagement(
    managementId: String,
    name: String,
    sourceTable: String,
    lastProcessedAt: Option[LocalDateTime],
    enabled: Boolean,
    createdAt: LocalDateTime,
    updatedAt: LocalDateTime,
)

/**
 * 自動仕訳パターン
 */
case class AutoJournalPattern(
    patternId: String,
    managementId: String,
    patternName: String,
    condition: Option[String],
    priority: Int,
    enabled: Boolean,
    createdAt: LocalDateTime,
    updatedAt: LocalDateTime,
)

/**
 * 自動仕訳パターン明細
 */
case class AutoJournalPatternDetail(
    patternId: String,
    lineNumber: Int,
    debitCreditType: DebitCreditType,
    accountCode: String,
    amountExpression: String,
    descriptionTemplate: Option[String],
    createdAt: LocalDateTime,
    updatedAt: LocalDateTime,
)

/**
 * 自動仕訳実行ログ
 */
case class AutoJournalExecutionLog(
    logId: Long,
    managementId: String,
    startTime: LocalDateTime,
    endTime: Option[LocalDateTime],
    totalCount: Int,
    successCount: Int,
    errorCount: Int,
    result: ExecutionResult,
    errorMessage: Option[String],
    createdAt: LocalDateTime,
):
  /** 完了しているかどうか */
  def isCompleted: Boolean = result != ExecutionResult.Running

  /** 成功かどうか */
  def isSuccess: Boolean = result == ExecutionResult.Success

  /** 部分成功かどうか */
  def isPartialSuccess: Boolean = result == ExecutionResult.Partial
