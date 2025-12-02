package com.example.accounting.infrastructure.in.http

import spray.json.*
import java.time.{Instant, LocalDate, LocalDateTime}
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

  // 監査ログ
  given auditLogResponseFormat: RootJsonFormat[AuditLogResponse] = jsonFormat12(AuditLogResponse.apply)
  given listAuditLogResponseFormat: RootJsonFormat[List[AuditLogResponse]] = listFormat[AuditLogResponse]
  given auditLogSearchRequestFormat: RootJsonFormat[AuditLogSearchRequest] = jsonFormat7(AuditLogSearchRequest.apply)

  // Instant 用フォーマット
  given instantFormat: JsonFormat[Instant] = new JsonFormat[Instant]:
    def write(instant: Instant): JsValue = JsString(instant.toString)
    def read(value: JsValue): Instant = value match
      case JsString(s) => Instant.parse(s)
      case _           => deserializationError("Instant expected")

  // 仕訳エントリ（イベントソーシング）
  given journalEntryDetailRequestFormat: RootJsonFormat[JournalEntryDetailRequest] =
    jsonFormat5(JournalEntryDetailRequest.apply)
  given listJournalEntryDetailRequestFormat: RootJsonFormat[List[JournalEntryDetailRequest]] =
    listFormat[JournalEntryDetailRequest]
  given journalEntryCreateRequestFormat: RootJsonFormat[JournalEntryCreateRequest] = {
    given ld: RootJsonFormat[List[JournalEntryDetailRequest]] = listJournalEntryDetailRequestFormat
    jsonFormat4(JournalEntryCreateRequest.apply)
  }
  given journalEntryApproveRequestFormat: RootJsonFormat[JournalEntryApproveRequest] =
    jsonFormat1(JournalEntryApproveRequest.apply)
  given journalEntryRejectRequestFormat: RootJsonFormat[JournalEntryRejectRequest] =
    jsonFormat2(JournalEntryRejectRequest.apply)
  given journalEntryDeleteRequestFormat: RootJsonFormat[JournalEntryDeleteRequest] =
    jsonFormat2(JournalEntryDeleteRequest.apply)
  given journalEntryDetailResponseFormat: RootJsonFormat[JournalEntryDetailResponse] =
    jsonFormat5(JournalEntryDetailResponse.apply)
  given listJournalEntryDetailResponseFormat: RootJsonFormat[List[JournalEntryDetailResponse]] =
    listFormat[JournalEntryDetailResponse]
  given journalEntryResponseFormat: RootJsonFormat[JournalEntryResponse] = {
    given ld: RootJsonFormat[List[JournalEntryDetailResponse]] = listJournalEntryDetailResponseFormat
    jsonFormat13(JournalEntryResponse.apply)
  }
  given listJournalEntryResponseFormat: RootJsonFormat[List[JournalEntryResponse]] = listFormat[JournalEntryResponse]
  given journalEntryEventResponseFormat: RootJsonFormat[JournalEntryEventResponse] =
    jsonFormat4(JournalEntryEventResponse.apply)
  given listJournalEntryEventResponseFormat: RootJsonFormat[List[JournalEntryEventResponse]] =
    listFormat[JournalEntryEventResponse]

  // 財務分析
  given profitabilityResponseFormat: RootJsonFormat[ProfitabilityResponse] =
    jsonFormat4(ProfitabilityResponse.apply)
  given efficiencyResponseFormat: RootJsonFormat[EfficiencyResponse] =
    jsonFormat2(EfficiencyResponse.apply)
  given safetyResponseFormat: RootJsonFormat[SafetyResponse] =
    jsonFormat3(SafetyResponse.apply)
  given financialAnalysisResponseFormat: RootJsonFormat[FinancialAnalysisResponse] = {
    given p: RootJsonFormat[ProfitabilityResponse] = profitabilityResponseFormat
    given e: RootJsonFormat[EfficiencyResponse] = efficiencyResponseFormat
    given s: RootJsonFormat[SafetyResponse] = safetyResponseFormat
    jsonFormat4(FinancialAnalysisResponse.apply)
  }
  given listFinancialAnalysisResponseFormat: RootJsonFormat[List[FinancialAnalysisResponse]] =
    listFormat[FinancialAnalysisResponse]
  given profitabilityChangeResponseFormat: RootJsonFormat[ProfitabilityChangeResponse] =
    jsonFormat4(ProfitabilityChangeResponse.apply)
  given efficiencyChangeResponseFormat: RootJsonFormat[EfficiencyChangeResponse] =
    jsonFormat2(EfficiencyChangeResponse.apply)
  given safetyChangeResponseFormat: RootJsonFormat[SafetyChangeResponse] =
    jsonFormat3(SafetyChangeResponse.apply)
  given ratioChangesResponseFormat: RootJsonFormat[RatioChangesResponse] = {
    given pc: RootJsonFormat[ProfitabilityChangeResponse] = profitabilityChangeResponseFormat
    given ec: RootJsonFormat[EfficiencyChangeResponse] = efficiencyChangeResponseFormat
    given sc: RootJsonFormat[SafetyChangeResponse] = safetyChangeResponseFormat
    jsonFormat3(RatioChangesResponse.apply)
  }
  given financialComparisonResponseFormat: RootJsonFormat[FinancialComparisonResponse] =
    new RootJsonFormat[FinancialComparisonResponse]:
      def write(obj: FinancialComparisonResponse): JsValue =
        val fields = scala.collection.mutable.Map[String, JsValue](
          "results" -> obj.results.map(financialAnalysisResponseFormat.write).toJson
        )
        obj.changes.foreach { c =>
          fields += ("changes" -> ratioChangesResponseFormat.write(c))
        }
        JsObject(fields.toMap)

      def read(value: JsValue): FinancialComparisonResponse =
        value.asJsObject.getFields("results", "changes") match
          case Seq(results, changes) =>
            FinancialComparisonResponse(
              results.convertTo[List[FinancialAnalysisResponse]],
              Some(changes.convertTo[RatioChangesResponse])
            )
          case Seq(results) =>
            FinancialComparisonResponse(
              results.convertTo[List[FinancialAnalysisResponse]],
              None
            )
          case _ => deserializationError("FinancialComparisonResponse expected")

object JsonFormats extends JsonFormats
