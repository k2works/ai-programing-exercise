package com.example.accounting.infrastructure.http

import spray.json.*
import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter
import com.example.accounting.infrastructure.http.dto.*

/**
 * JSON フォーマット定義
 */
trait JsonFormats extends DefaultJsonProtocol:

  // LocalDate 用フォーマット
  given localDateFormat: JsonFormat[LocalDate] = new JsonFormat[LocalDate]:
    private val formatter = DateTimeFormatter.ISO_LOCAL_DATE

    def write(date: LocalDate): JsValue = JsString(date.format(formatter))

    def read(value: JsValue): LocalDate = value match
      case JsString(s) => LocalDate.parse(s, formatter)
      case _           => deserializationError("LocalDate expected")

  // LocalDateTime 用フォーマット
  given localDateTimeFormat: JsonFormat[LocalDateTime] = new JsonFormat[LocalDateTime]:
    private val formatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME

    def write(dateTime: LocalDateTime): JsValue = JsString(dateTime.format(formatter))

    def read(value: JsValue): LocalDateTime = value match
      case JsString(s) => LocalDateTime.parse(s, formatter)
      case _           => deserializationError("LocalDateTime expected")

  // エラーレスポンス
  given errorResponseFormat: RootJsonFormat[ErrorResponse] = jsonFormat3(ErrorResponse.apply)

  // 勘定科目
  given accountRequestFormat: RootJsonFormat[AccountRequest] = jsonFormat12(AccountRequest.apply)
  given accountResponseFormat: RootJsonFormat[AccountResponse] = jsonFormat13(AccountResponse.apply)

  // 仕訳明細
  given journalDetailRequestFormat: RootJsonFormat[JournalDetailRequest] = jsonFormat2(JournalDetailRequest.apply)
  given journalDetailResponseFormat: RootJsonFormat[JournalDetailResponse] = jsonFormat3(JournalDetailResponse.apply)

  // 仕訳貸借明細
  given journalDebitCreditDetailRequestFormat: RootJsonFormat[JournalDebitCreditDetailRequest] =
    jsonFormat20(JournalDebitCreditDetailRequest.apply)
  given journalDebitCreditDetailResponseFormat: RootJsonFormat[JournalDebitCreditDetailResponse] =
    jsonFormat21(JournalDebitCreditDetailResponse.apply)

  // 仕訳
  given journalRequestFormat: RootJsonFormat[JournalRequest] = jsonFormat13(JournalRequest.apply)
  given journalResponseFormat: RootJsonFormat[JournalResponse] = jsonFormat16(JournalResponse.apply)
  given journalValidationResponseFormat: RootJsonFormat[JournalValidationResponse] =
    jsonFormat4(JournalValidationResponse.apply)

  // 財務諸表
  given financialStatementItemResponseFormat: RootJsonFormat[FinancialStatementItemResponse] =
    jsonFormat4(FinancialStatementItemResponse.apply)
  given balanceSheetResponseFormat: RootJsonFormat[BalanceSheetResponse] = jsonFormat9(BalanceSheetResponse.apply)
  given incomeStatementResponseFormat: RootJsonFormat[IncomeStatementResponse] =
    jsonFormat9(IncomeStatementResponse.apply)
  given financialRatiosResponseFormat: RootJsonFormat[FinancialRatiosResponse] =
    jsonFormat7(FinancialRatiosResponse.apply)

object JsonFormats extends JsonFormats
