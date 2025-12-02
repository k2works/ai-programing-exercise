package com.example.financial.domain

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.EitherValues
import com.example.common.domain.*
import java.time.LocalDate

class JournalSpec extends AnyFlatSpec with Matchers with EitherValues:

  "Journal.create" should "借方合計と貸方合計が一致する仕訳を作成できる" in {
    val fiscalYear = FiscalYear.unsafeFrom(2024)
    val entries = List(
      JournalEntry.debit(AccountCode.unsafeFrom("1100"), Money(10000), "現金増加"),
      JournalEntry.credit(AccountCode.unsafeFrom("4100"), Money(10000), "売上計上")
    )

    val result = Journal.create(
      journalDate = LocalDate.of(2024, 1, 15),
      description = "現金売上",
      fiscalYear = fiscalYear,
      entries = entries
    )

    result.isRight shouldBe true
    val journal = result.value
    journal.entries.size shouldBe 2
  }

  it should "借方合計と貸方合計が一致しない場合はエラーを返す" in {
    val fiscalYear = FiscalYear.unsafeFrom(2024)
    val entries = List(
      JournalEntry.debit(AccountCode.unsafeFrom("1100"), Money(10000), ""),
      JournalEntry.credit(AccountCode.unsafeFrom("4100"), Money(5000), "")
    )

    val result = Journal.create(
      journalDate = LocalDate.of(2024, 1, 15),
      description = "不正な仕訳",
      fiscalYear = fiscalYear,
      entries = entries
    )

    result.isLeft shouldBe true
    result.left.value should include("借方合計")
  }

  it should "明細が空の場合はエラーを返す" in {
    val fiscalYear = FiscalYear.unsafeFrom(2024)

    val result = Journal.create(
      journalDate = LocalDate.of(2024, 1, 15),
      description = "空の仕訳",
      fiscalYear = fiscalYear,
      entries = List.empty
    )

    result.isLeft shouldBe true
    result.left.value should include("必要")
  }

  it should "借方明細のみの場合はエラーを返す" in {
    val fiscalYear = FiscalYear.unsafeFrom(2024)
    val entries = List(
      JournalEntry.debit(AccountCode.unsafeFrom("1100"), Money(10000), "")
    )

    val result = Journal.create(
      journalDate = LocalDate.of(2024, 1, 15),
      description = "借方のみ",
      fiscalYear = fiscalYear,
      entries = entries
    )

    result.isLeft shouldBe true
  }

  "JournalEntry.debit" should "借方明細を作成できる" in {
    val entry = JournalEntry.debit(
      AccountCode.unsafeFrom("1100"),
      Money(10000),
      "現金"
    )

    entry.debitAmount.value shouldBe BigDecimal(10000)
    entry.creditAmount.value shouldBe BigDecimal(0)
  }

  "JournalEntry.credit" should "貸方明細を作成できる" in {
    val entry = JournalEntry.credit(
      AccountCode.unsafeFrom("4100"),
      Money(10000),
      "売上"
    )

    entry.debitAmount.value shouldBe BigDecimal(0)
    entry.creditAmount.value shouldBe BigDecimal(10000)
  }
