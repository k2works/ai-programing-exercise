package com.example.accounting

import com.example.db.DatabaseSpec
import org.scalatest.BeforeAndAfterEach
import scalikejdbc.*

import java.time.LocalDate

/**
 * 複式簿記の貸借平衡チェック機能のテスト
 */
class JournalBalanceCheckSpec extends DatabaseSpec with BeforeAndAfterEach:

  override def beforeEach(): Unit =
    super.beforeEach()

  behavior of "複式簿記 貸借平衡チェック（2層構造）"

  it should "貸借が平衡している仕訳をビューで確認できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      // テスト用勘定科目を登録
      insertTestAccounts("BC1")

      // 貸借平衡の仕訳を登録
      sql"""
        INSERT INTO "仕訳エントリ" ("伝票番号", "仕訳日", "摘要", "合計金額", "作成者")
        VALUES (${"BC1-001"}, ${LocalDate.of(2025, 1, 1)}, ${"テスト仕訳"}, ${BigDecimal("100000.00")}, ${"user001"})
      """.update.apply()

      sql"""
        INSERT INTO "仕訳明細" ("伝票番号", "行番号", "勘定科目コード", "借方金額", "貸方金額", "摘要")
        VALUES (${"BC1-001"}, 1, ${"BC1-1100"}, ${BigDecimal("100000.00")}, ${BigDecimal("0.00")}, ${"借方"})
      """.update.apply()

      sql"""
        INSERT INTO "仕訳明細" ("伝票番号", "行番号", "勘定科目コード", "借方金額", "貸方金額", "摘要")
        VALUES (${"BC1-001"}, 2, ${"BC1-4100"}, ${BigDecimal("0.00")}, ${BigDecimal("100000.00")}, ${"貸方"})
      """.update.apply()

      // ビューで確認
      val result = sql"""
        SELECT "伝票番号", debit_total, credit_total, diff, is_balanced
        FROM "v_仕訳エントリ貸借チェック"
        WHERE "伝票番号" = ${"BC1-001"}
      """.map { rs =>
        (
          rs.string("伝票番号"),
          BigDecimal(rs.bigDecimal("debit_total")),
          BigDecimal(rs.bigDecimal("credit_total")),
          BigDecimal(rs.bigDecimal("diff")),
          rs.boolean("is_balanced"),
        )
      }.single.apply()

      result should not be None
      val (voucherNo, debit, credit, diff, isBalanced) = result.get
      voucherNo shouldBe "BC1-001"
      debit shouldBe BigDecimal("100000.00")
      credit shouldBe BigDecimal("100000.00")
      diff shouldBe BigDecimal("0.00")
      isBalanced shouldBe true
    }
  }

  behavior of "複式簿記 貸借平衡チェック（3層構造）"

  it should "貸借平衡している仕訳をビューで確認できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      // テスト用勘定科目を登録
      insertTestAccounts("BC2")

      // 3層構造の仕訳を登録
      val journalNo = "BC2-3L-001"
      sql"""
        INSERT INTO "仕訳" (
          "仕訳伝票番号", "起票日", "入力日", "決算仕訳フラグ", "単振フラグ",
          "仕訳伝票区分", "定期計上フラグ", "赤伝フラグ"
        ) VALUES (${journalNo}, ${LocalDate.of(2025, 1, 1)}, ${LocalDate.of(2025, 1, 1)}, 0, 1, 0, 0, 0)
      """.update.apply()

      sql"""
        INSERT INTO "仕訳明細3" ("仕訳伝票番号", "仕訳行番号", "行摘要")
        VALUES (${journalNo}, 1, ${"テスト"})
      """.update.apply()

      sql"""
        INSERT INTO "仕訳貸借明細" (
          "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
          "通貨コード", "為替レート", "勘定科目コード",
          "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
        ) VALUES (${journalNo}, 1, ${"D"}, ${"JPY"}, ${BigDecimal("1.00")}, ${"BC2-1100"},
                  ${BigDecimal("50000.00")}, ${BigDecimal("50000.00")}, 0)
      """.update.apply()

      sql"""
        INSERT INTO "仕訳貸借明細" (
          "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
          "通貨コード", "為替レート", "勘定科目コード",
          "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
        ) VALUES (${journalNo}, 1, ${"C"}, ${"JPY"}, ${BigDecimal("1.00")}, ${"BC2-4100"},
                  ${BigDecimal("50000.00")}, ${BigDecimal("50000.00")}, 0)
      """.update.apply()

      // ビューで確認
      val result = sql"""
        SELECT "仕訳伝票番号", debit_total, credit_total, diff, is_balanced
        FROM "v_仕訳貸借チェック"
        WHERE "仕訳伝票番号" = ${journalNo}
      """.map { rs =>
        (
          rs.string("仕訳伝票番号"),
          BigDecimal(rs.bigDecimal("debit_total")),
          BigDecimal(rs.bigDecimal("credit_total")),
          BigDecimal(rs.bigDecimal("diff")),
          rs.boolean("is_balanced"),
        )
      }.single.apply()

      result should not be None
      val (voucherNo, debit, credit, diff, isBalanced) = result.get
      isBalanced shouldBe true
      diff shouldBe BigDecimal("0.00")
    }
  }

  it should "validate_journal_balance関数で特定の仕訳を検証できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      // テスト用勘定科目を登録
      insertTestAccounts("BC3")

      // 3層構造の仕訳を登録
      val journalNo = "BC3-FUNC-001"
      sql"""
        INSERT INTO "仕訳" (
          "仕訳伝票番号", "起票日", "入力日", "決算仕訳フラグ", "単振フラグ",
          "仕訳伝票区分", "定期計上フラグ", "赤伝フラグ"
        ) VALUES (${journalNo}, ${LocalDate.of(2025, 1, 1)}, ${LocalDate.of(2025, 1, 1)}, 0, 1, 0, 0, 0)
      """.update.apply()

      sql"""
        INSERT INTO "仕訳明細3" ("仕訳伝票番号", "仕訳行番号", "行摘要")
        VALUES (${journalNo}, 1, ${"関数テスト"})
      """.update.apply()

      sql"""
        INSERT INTO "仕訳貸借明細" (
          "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
          "通貨コード", "為替レート", "勘定科目コード",
          "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
        ) VALUES (${journalNo}, 1, ${"D"}, ${"JPY"}, ${BigDecimal("1.00")}, ${"BC3-1100"},
                  ${BigDecimal("75000.00")}, ${BigDecimal("75000.00")}, 0)
      """.update.apply()

      sql"""
        INSERT INTO "仕訳貸借明細" (
          "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
          "通貨コード", "為替レート", "勘定科目コード",
          "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
        ) VALUES (${journalNo}, 1, ${"C"}, ${"JPY"}, ${BigDecimal("1.00")}, ${"BC3-4100"},
                  ${BigDecimal("75000.00")}, ${BigDecimal("75000.00")}, 0)
      """.update.apply()

      // 関数で検証
      val result = sql"""
        SELECT * FROM validate_journal_balance(${journalNo})
      """.map { rs =>
        (
          rs.boolean("is_balanced"),
          BigDecimal(rs.bigDecimal("debit_total")),
          BigDecimal(rs.bigDecimal("credit_total")),
          BigDecimal(rs.bigDecimal("diff")),
        )
      }.single.apply()

      result should not be None
      val (isBalanced, debit, credit, diff) = result.get
      isBalanced shouldBe true
      debit shouldBe BigDecimal("75000.00")
      credit shouldBe BigDecimal("75000.00")
      diff shouldBe BigDecimal("0.00")
    }
  }

  it should "check_journal_balance関数で不平衡仕訳を検出できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      // テスト用勘定科目を登録
      insertTestAccounts("BC4")

      // 貸借不平衡の仕訳を登録（意図的に不平衡にする）
      val journalNo = "BC4-UNBAL-001"
      sql"""
        INSERT INTO "仕訳" (
          "仕訳伝票番号", "起票日", "入力日", "決算仕訳フラグ", "単振フラグ",
          "仕訳伝票区分", "定期計上フラグ", "赤伝フラグ"
        ) VALUES (${journalNo}, ${LocalDate.of(2025, 1, 1)}, ${LocalDate.of(2025, 1, 1)}, 0, 1, 0, 0, 0)
      """.update.apply()

      sql"""
        INSERT INTO "仕訳明細3" ("仕訳伝票番号", "仕訳行番号", "行摘要")
        VALUES (${journalNo}, 1, ${"不平衡テスト"})
      """.update.apply()

      // 借方のみ登録（貸借不平衡）
      sql"""
        INSERT INTO "仕訳貸借明細" (
          "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
          "通貨コード", "為替レート", "勘定科目コード",
          "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
        ) VALUES (${journalNo}, 1, ${"D"}, ${"JPY"}, ${BigDecimal("1.00")}, ${"BC4-1100"},
                  ${BigDecimal("100000.00")}, ${BigDecimal("100000.00")}, 0)
      """.update.apply()

      // 関数で不平衡仕訳を検出
      val unbalanced = sql"""
        SELECT * FROM check_journal_balance()
        WHERE journal_no = ${journalNo}
      """.map { rs =>
        (
          rs.string("journal_no"),
          BigDecimal(rs.bigDecimal("debit_total")),
          BigDecimal(rs.bigDecimal("credit_total")),
          BigDecimal(rs.bigDecimal("diff")),
        )
      }.single.apply()

      unbalanced should not be None
      val (voucherNo, debit, credit, diff) = unbalanced.get
      voucherNo shouldBe journalNo
      debit shouldBe BigDecimal("100000.00")
      credit shouldBe BigDecimal("0.00")
      diff shouldBe BigDecimal("100000.00")
    }
  }

  it should "不平衡仕訳ビューで問題のある仕訳のみ表示できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      // テスト用勘定科目を登録
      insertTestAccounts("BC5")

      // 平衡仕訳
      val balancedNo = "BC5-BAL-001"
      sql"""
        INSERT INTO "仕訳" (
          "仕訳伝票番号", "起票日", "入力日", "決算仕訳フラグ", "単振フラグ",
          "仕訳伝票区分", "定期計上フラグ", "赤伝フラグ"
        ) VALUES (${balancedNo}, ${LocalDate.of(2025, 1, 1)}, ${LocalDate.of(2025, 1, 1)}, 0, 1, 0, 0, 0)
      """.update.apply()

      sql"""
        INSERT INTO "仕訳明細3" ("仕訳伝票番号", "仕訳行番号", "行摘要")
        VALUES (${balancedNo}, 1, ${"平衡"})
      """.update.apply()

      sql"""
        INSERT INTO "仕訳貸借明細" (
          "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
          "通貨コード", "為替レート", "勘定科目コード",
          "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
        ) VALUES (${balancedNo}, 1, ${"D"}, ${"JPY"}, ${BigDecimal("1.00")}, ${"BC5-1100"},
                  ${BigDecimal("10000.00")}, ${BigDecimal("10000.00")}, 0)
      """.update.apply()

      sql"""
        INSERT INTO "仕訳貸借明細" (
          "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
          "通貨コード", "為替レート", "勘定科目コード",
          "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
        ) VALUES (${balancedNo}, 1, ${"C"}, ${"JPY"}, ${BigDecimal("1.00")}, ${"BC5-4100"},
                  ${BigDecimal("10000.00")}, ${BigDecimal("10000.00")}, 0)
      """.update.apply()

      // 不平衡仕訳
      val unbalancedNo = "BC5-UNBAL-001"
      sql"""
        INSERT INTO "仕訳" (
          "仕訳伝票番号", "起票日", "入力日", "決算仕訳フラグ", "単振フラグ",
          "仕訳伝票区分", "定期計上フラグ", "赤伝フラグ"
        ) VALUES (${unbalancedNo}, ${LocalDate.of(2025, 1, 1)}, ${LocalDate.of(2025, 1, 1)}, 0, 1, 0, 0, 0)
      """.update.apply()

      sql"""
        INSERT INTO "仕訳明細3" ("仕訳伝票番号", "仕訳行番号", "行摘要")
        VALUES (${unbalancedNo}, 1, ${"不平衡"})
      """.update.apply()

      sql"""
        INSERT INTO "仕訳貸借明細" (
          "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
          "通貨コード", "為替レート", "勘定科目コード",
          "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
        ) VALUES (${unbalancedNo}, 1, ${"D"}, ${"JPY"}, ${BigDecimal("1.00")}, ${"BC5-1100"},
                  ${BigDecimal("20000.00")}, ${BigDecimal("20000.00")}, 0)
      """.update.apply()

      // 不平衡仕訳ビューで確認
      val unbalancedList = sql"""
        SELECT "仕訳伝票番号" FROM "v_不平衡仕訳"
        WHERE "仕訳伝票番号" LIKE 'BC5%'
      """.map(_.string("仕訳伝票番号")).list.apply()

      unbalancedList should have size 1
      unbalancedList.head shouldBe unbalancedNo
    }
  }

  /**
   * テスト用勘定科目を登録するヘルパーメソッド
   */
  private def insertTestAccounts(prefix: String)(implicit session: DBSession): Unit =
    val accounts = List(
      (s"$prefix-1100", "現金", "資産"),
      (s"$prefix-4100", "売上", "収益"),
    )

    accounts.foreach { case (code, name, accountType) =>
      sql"""
        INSERT INTO "勘定科目マスタ" (
          "勘定科目コード", "勘定科目名", "勘定科目種別", "残高"
        ) VALUES (
          ${code}, ${name}, ${accountType}::account_type, ${BigDecimal("0")}
        )
      """.update.apply()
    }
