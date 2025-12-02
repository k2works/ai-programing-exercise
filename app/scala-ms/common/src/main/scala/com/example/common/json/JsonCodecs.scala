package com.example.common.json

import io.circe.{Decoder, Encoder, Json}
import com.example.common.domain.*
import java.time.{LocalDate, LocalDateTime, YearMonth}
import java.time.format.DateTimeFormatter

/**
 * 共通の JSON エンコーダー/デコーダー
 */
object JsonCodecs:

  // LocalDate
  given Encoder[LocalDate] = Encoder.encodeString.contramap(_.toString)
  given Decoder[LocalDate] = Decoder.decodeString.emap { str =>
    try Right(LocalDate.parse(str))
    catch case _: Exception => Left(s"Invalid date format: $str")
  }

  // LocalDateTime
  given Encoder[LocalDateTime] = Encoder.encodeString.contramap(_.toString)
  given Decoder[LocalDateTime] = Decoder.decodeString.emap { str =>
    try Right(LocalDateTime.parse(str))
    catch case _: Exception => Left(s"Invalid datetime format: $str")
  }

  // YearMonth
  given Encoder[YearMonth] = Encoder.encodeString.contramap(_.toString)
  given Decoder[YearMonth] = Decoder.decodeString.emap { str =>
    try Right(YearMonth.parse(str))
    catch case _: Exception => Left(s"Invalid year-month format: $str")
  }

  // FiscalYear
  given Encoder[FiscalYear] = Encoder.encodeInt.contramap(_.value)
  given Decoder[FiscalYear] = Decoder.decodeInt.emap(FiscalYear.apply)

  // AccountCode
  given Encoder[AccountCode] = Encoder.encodeString.contramap(_.value)
  given Decoder[AccountCode] = Decoder.decodeString.emap(AccountCode.apply)

  // Money
  given Encoder[Money] = Encoder.encodeBigDecimal.contramap(_.value)
  given Decoder[Money] = Decoder.decodeBigDecimal.map(Money.apply)

  // AccountType
  given Encoder[AccountType] = Encoder.encodeString.contramap(_.toString.toLowerCase)
  given Decoder[AccountType] = Decoder.decodeString.emap(AccountType.fromString)

  // AuditAction
  given Encoder[AuditAction] = Encoder.encodeString.contramap(_.toString)
  given Decoder[AuditAction] = Decoder.decodeString.emap {
    case "Insert" => Right(AuditAction.Insert)
    case "Update" => Right(AuditAction.Update)
    case "Delete" => Right(AuditAction.Delete)
    case other    => Left(s"Unknown audit action: $other")
  }

  // DomainError
  given Encoder[DomainError] = Encoder.instance { error =>
    Json.obj(
      "type" -> Json.fromString(error.getClass.getSimpleName),
      "message" -> Json.fromString(error.message)
    )
  }
