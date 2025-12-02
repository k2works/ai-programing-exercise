package com.example.common.domain

import java.time.{LocalDate, LocalDateTime, YearMonth}

/**
 * 共通の型定義
 *
 * 境界付けられたコンテキスト間で共有される基本的な型を定義します。
 */

/**
 * 会計年度を表す値オブジェクト
 */
opaque type FiscalYear = Int

object FiscalYear:
  def apply(year: Int): Either[String, FiscalYear] =
    if year >= 1900 && year <= 2100 then Right(year)
    else Left(s"Invalid fiscal year: $year")

  def unsafeFrom(year: Int): FiscalYear = year

  extension (fy: FiscalYear)
    def value: Int = fy
    def next: FiscalYear = fy + 1
    def previous: FiscalYear = fy - 1

/**
 * 勘定科目コードを表す値オブジェクト
 */
opaque type AccountCode = String

object AccountCode:
  def apply(code: String): Either[String, AccountCode] =
    if code.nonEmpty && code.matches("^[0-9]{1,10}$") then Right(code)
    else Left(s"Invalid account code: $code")

  def unsafeFrom(code: String): AccountCode = code

  extension (ac: AccountCode)
    def value: String = ac
    def prefix(length: Int): String = ac.take(length)

/**
 * 金額を表す値オブジェクト（不変）
 */
opaque type Money = BigDecimal

object Money:
  val Zero: Money = BigDecimal(0)

  def apply(amount: BigDecimal): Money = amount.setScale(2, BigDecimal.RoundingMode.HALF_UP)
  def apply(amount: Double): Money = BigDecimal(amount).setScale(2, BigDecimal.RoundingMode.HALF_UP)
  def apply(amount: Long): Money = BigDecimal(amount)
  def apply(amount: Int): Money = BigDecimal(amount)

  extension (m: Money)
    def value: BigDecimal = m
    def +(other: Money): Money = Money(m + other)
    def -(other: Money): Money = Money(m - other)
    def *(factor: BigDecimal): Money = Money(m * factor)
    def /(divisor: BigDecimal): Money = Money(m / divisor)
    def negate: Money = Money(-m)
    def isZero: Boolean = m == BigDecimal(0)
    def isPositive: Boolean = m > BigDecimal(0)
    def isNegative: Boolean = m < BigDecimal(0)

/**
 * 勘定科目種別
 */
enum AccountType:
  case Asset      // 資産
  case Liability  // 負債
  case Equity     // 純資産
  case Revenue    // 収益
  case Expense    // 費用

object AccountType:
  def fromString(s: String): Either[String, AccountType] = s.toLowerCase match
    case "asset" | "資産"     => Right(Asset)
    case "liability" | "負債"  => Right(Liability)
    case "equity" | "純資産"   => Right(Equity)
    case "revenue" | "収益"    => Right(Revenue)
    case "expense" | "費用"    => Right(Expense)
    case _                     => Left(s"Unknown account type: $s")

/**
 * 監査アクション
 */
enum AuditAction:
  case Insert
  case Update
  case Delete

/**
 * エラー型の共通基底
 */
sealed trait DomainError:
  def message: String

case class ValidationError(message: String) extends DomainError
case class NotFoundError(message: String) extends DomainError
case class ConflictError(message: String) extends DomainError
case class InternalError(message: String) extends DomainError
