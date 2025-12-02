package com.example.accounting.infrastructure.in.http

import spray.json.*
import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter
import com.example.accounting.infrastructure.in.http.dto.*

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
  given listJournalDetailRequestFormat: RootJsonFormat[List[JournalDetailRequest]] = listFormat[JournalDetailRequest]
  given journalDetailResponseFormat: RootJsonFormat[JournalDetailResponse] = jsonFormat3(JournalDetailResponse.apply)
  given listJournalDetailResponseFormat: RootJsonFormat[List[JournalDetailResponse]] = listFormat[JournalDetailResponse]

  // 仕訳貸借明細
  given journalDebitCreditDetailRequestFormat: RootJsonFormat[JournalDebitCreditDetailRequest] =
    jsonFormat20(JournalDebitCreditDetailRequest.apply)
  given listJournalDebitCreditDetailRequestFormat: RootJsonFormat[List[JournalDebitCreditDetailRequest]] =
    listFormat[JournalDebitCreditDetailRequest]
  given journalDebitCreditDetailResponseFormat: RootJsonFormat[JournalDebitCreditDetailResponse] =
    jsonFormat21(JournalDebitCreditDetailResponse.apply)
  given listJournalDebitCreditDetailResponseFormat: RootJsonFormat[List[JournalDebitCreditDetailResponse]] =
    listFormat[JournalDebitCreditDetailResponse]

  // 仕訳
  given journalRequestFormat: RootJsonFormat[JournalRequest] = {
    given ldr: RootJsonFormat[List[JournalDetailRequest]] = listJournalDetailRequestFormat
    given ldcr: RootJsonFormat[List[JournalDebitCreditDetailRequest]] = listJournalDebitCreditDetailRequestFormat
    jsonFormat13(JournalRequest.apply)
  }
  given journalResponseFormat: RootJsonFormat[JournalResponse] = {
    given ld: RootJsonFormat[List[JournalDetailResponse]] = listJournalDetailResponseFormat
    given ldc: RootJsonFormat[List[JournalDebitCreditDetailResponse]] = listJournalDebitCreditDetailResponseFormat
    jsonFormat16(JournalResponse.apply)
  }
  given listJournalResponseFormat: RootJsonFormat[List[JournalResponse]] = listFormat[JournalResponse]
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
