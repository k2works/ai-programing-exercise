package com.example.accounting

import com.example.db.DatabaseSpec
import org.scalatest.BeforeAndAfterEach
import scalikejdbc.*

import java.time.LocalDate

/**
 * 仕訳エントリのテスト（2層構造）
 *
 * このテストでは、仕訳エントリに対するCRUD操作と複式簿記の原理を検証します。
 * DatabaseSpecを継承してTestcontainersを使用し、
 * 実際のPostgreSQLコンテナでテストを実行します。
 */
class JournalEntrySpec extends DatabaseSpec with BeforeAndAfterEach:

  override def beforeEach(): Unit =
    super.beforeEach()

  behavior of "仕訳エントリ CRUD操作"

  it should "仕訳エントリを登録できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      // テスト用勘定科目を登録
      insertTestAccounts("JE1")

      // 1. 仕訳エントリを作成
      val entryNo = sql"""
        INSERT INTO "仕訳エントリ" ("伝票番号", "仕訳日", "摘要", "合計金額", "作成者")
        VALUES (${"JE1-240001"}, ${LocalDate.of(2024, 1, 15)}, ${"現金売上"}, ${BigDecimal("110000.00")}, ${"user001"})
        RETURNING "伝票番号"
      """.map(_.string("伝票番号")).single.apply()

      entryNo shouldBe Some("JE1-240001")

      // 2. 仕訳明細を作成（借方：現金、貸方：売上+消費税）
      // 借方：現金 110,000
      sql"""
        INSERT INTO "仕訳明細" (
          "伝票番号", "行番号", "勘定科目コード",
          "借方金額", "貸方金額", "摘要"
        ) VALUES (${"JE1-240001"}, ${1}, ${"JE1-1100"}, ${BigDecimal("110000.00")}, ${BigDecimal(0)}, ${"商品売上による現金収入"})
      """.update.apply()

      // 貸方：売上 100,000
      sql"""
        INSERT INTO "仕訳明細" (
          "伝票番号", "行番号", "勘定科目コード",
          "借方金額", "貸方金額", "摘要"
        ) VALUES (${"JE1-240001"}, ${2}, ${"JE1-4100"}, ${BigDecimal(0)}, ${BigDecimal("100000.00")}, ${"商品売上"})
      """.update.apply()

      // 貸方：仮受消費税 10,000
      sql"""
        INSERT INTO "仕訳明細" (
          "伝票番号", "行番号", "勘定科目コード",
          "借方金額", "貸方金額", "摘要"
        ) VALUES (${"JE1-240001"}, ${3}, ${"JE1-2120"}, ${BigDecimal(0)}, ${BigDecimal("10000.00")}, ${"消費税"})
      """.update.apply()

      // 3. 借方・貸方の合計を検証（複式簿記の原理）
      val balance = sql"""
        SELECT
          SUM("借方金額") as debit_total,
          SUM("貸方金額") as credit_total
        FROM "仕訳明細"
        WHERE "伝票番号" = ${"JE1-240001"}
      """.map { rs =>
        (BigDecimal(rs.bigDecimal("debit_total")), BigDecimal(rs.bigDecimal("credit_total")))
      }.single.apply()

      balance should not be None
      val (debitTotal, creditTotal) = balance.get

      // 複式簿記の原理：借方合計 = 貸方合計
      debitTotal shouldBe creditTotal
      debitTotal shouldBe BigDecimal("110000.00")
    }
  }

  it should "仕訳エントリを更新できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      // テスト用勘定科目を登録
      insertTestAccounts("JE2")

      // 1. テストデータを登録
      insertJournalEntry(
        "JE2-240001",
        LocalDate.of(2024, 1, 15),
        "現金売上",
        BigDecimal("110000.00"),
        "user001"
      )

      // 2. 摘要を更新
      val updated = sql"""
        UPDATE "仕訳エントリ"
        SET "摘要" = ${"現金売上（修正）"}, "更新者" = ${"user002"}, "更新日時" = CURRENT_TIMESTAMP
        WHERE "伝票番号" = ${"JE2-240001"}
      """.update.apply()

      updated shouldBe 1

      // 3. 更新されたか検証
      val result = sql"""
        SELECT "摘要", "更新者"
        FROM "仕訳エントリ"
        WHERE "伝票番号" = ${"JE2-240001"}
      """.map { rs =>
        (rs.string("摘要"), rs.string("更新者"))
      }.single.apply()

      result should not be None
      val (description, updater) = result.get
      description shouldBe "現金売上（修正）"
      updater shouldBe "user002"
    }
  }

  it should "仕訳エントリを削除できる（明細も連鎖削除）" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      // テスト用勘定科目を登録
      insertTestAccounts("JE3")

      // 1. テストデータを登録
      insertJournalEntry(
        "JE3-240001",
        LocalDate.of(2024, 1, 15),
        "現金売上",
        BigDecimal("110000.00"),
        "user001"
      )

      // 仕訳明細も登録
      insertJournalDetail("JE3-240001", 1, "JE3-1100", BigDecimal("110000"), BigDecimal(0), "現金")
      insertJournalDetail("JE3-240001", 2, "JE3-4100", BigDecimal(0), BigDecimal("110000"), "売上")

      // 2. 仕訳エントリを削除
      val deleted = sql"""
        DELETE FROM "仕訳エントリ"
        WHERE "伝票番号" = ${"JE3-240001"}
      """.update.apply()

      deleted shouldBe 1

      // 3. データが削除されたか検証
      val entryCount = sql"""
        SELECT COUNT(*) as count FROM "仕訳エントリ" WHERE "伝票番号" = ${"JE3-240001"}
      """.map(_.int("count")).single.apply()

      entryCount shouldBe Some(0)

      // 明細も連鎖削除されているか確認
      val detailCount = sql"""
        SELECT COUNT(*) as count FROM "仕訳明細" WHERE "伝票番号" = ${"JE3-240001"}
      """.map(_.int("count")).single.apply()

      detailCount shouldBe Some(0)
    }
  }

  it should "複雑な仕訳エントリ（売掛金回収と振込手数料）を登録できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      // テスト用勘定科目を登録
      insertTestAccounts("JE4")

      // 1. 仕訳エントリを作成
      insertJournalEntry(
        "JE4-240002",
        LocalDate.of(2024, 1, 20),
        "売掛金回収と振込手数料",
        BigDecimal("105000.00"),
        "user001"
      )

      // 2. 複雑な仕訳明細を作成
      // 借方：普通預金 104,500（振込手数料差引後）
      insertJournalDetail(
        "JE4-240002",
        1,
        "JE4-1200",
        BigDecimal("104500.00"),
        BigDecimal(0),
        "売掛金回収（振込手数料差引後）"
      )

      // 借方：支払手数料 500
      insertJournalDetail(
        "JE4-240002",
        2,
        "JE4-6200",
        BigDecimal("500.00"),
        BigDecimal(0),
        "振込手数料"
      )

      // 貸方：売掛金 105,000
      insertJournalDetail(
        "JE4-240002",
        3,
        "JE4-1300",
        BigDecimal(0),
        BigDecimal("105000.00"),
        "売掛金回収"
      )

      // 3. 借方・貸方の合計を検証
      val balance = sql"""
        SELECT
          SUM("借方金額") as debit_total,
          SUM("貸方金額") as credit_total
        FROM "仕訳明細"
        WHERE "伝票番号" = ${"JE4-240002"}
      """.map { rs =>
        (BigDecimal(rs.bigDecimal("debit_total")), BigDecimal(rs.bigDecimal("credit_total")))
      }.single.apply()

      balance should not be None
      val (debitTotal, creditTotal) = balance.get

      // 複式簿記の原理：借方合計 = 貸方合計
      debitTotal shouldBe creditTotal
      debitTotal shouldBe BigDecimal("105000.00")
    }
  }

  /**
   * テスト用勘定科目を登録するヘルパーメソッド
   */
  private def insertTestAccounts(prefix: String)(implicit session: DBSession): Unit =
    val accounts = List(
      (s"$prefix-1100", "現金", "資産"),
      (s"$prefix-1200", "普通預金", "資産"),
      (s"$prefix-1300", "売掛金", "資産"),
      (s"$prefix-2120", "仮受消費税", "負債"),
      (s"$prefix-4100", "売上", "収益"),
      (s"$prefix-6200", "支払手数料", "費用"),
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

  /**
   * 仕訳エントリを登録するヘルパーメソッド
   */
  private def insertJournalEntry(
      voucherNumber: String,
      journalDate: LocalDate,
      description: String,
      totalAmount: BigDecimal,
      creator: String,
  )(implicit session: DBSession): String =
    sql"""
      INSERT INTO "仕訳エントリ" ("伝票番号", "仕訳日", "摘要", "合計金額", "作成者")
      VALUES (${voucherNumber}, ${journalDate}, ${description}, ${totalAmount}, ${creator})
      RETURNING "伝票番号"
    """.map(_.string("伝票番号")).single.apply().get

  /**
   * 仕訳明細を登録するヘルパーメソッド
   */
  private def insertJournalDetail(
      voucherNumber: String,
      lineNumber: Int,
      accountCode: String,
      debitAmount: BigDecimal,
      creditAmount: BigDecimal,
      description: String,
  )(implicit session: DBSession): Unit =
    sql"""
      INSERT INTO "仕訳明細" (
        "伝票番号", "行番号", "勘定科目コード",
        "借方金額", "貸方金額", "摘要"
      ) VALUES (
        ${voucherNumber}, ${lineNumber}, ${accountCode},
        ${debitAmount}, ${creditAmount}, ${description}
      )
    """.update.apply()
