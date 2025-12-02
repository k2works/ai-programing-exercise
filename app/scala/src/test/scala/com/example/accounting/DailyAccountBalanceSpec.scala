package com.example.accounting

import com.example.accounting.domain.balance.*
import com.example.accounting.infrastructure.out.persistence.balance.BalanceRepository
import com.example.db.DatabaseSpec
import org.scalatest.BeforeAndAfterEach
import scalikejdbc.*

import java.time.LocalDate

/**
 * 日次勘定科目残高テーブルのテスト
 */
class DailyAccountBalanceSpec extends DatabaseSpec with BeforeAndAfterEach:

  override def beforeEach(): Unit =
    super.beforeEach()

  private def clearTestData()(implicit session: DBSession): Unit =
    sql"""DELETE FROM "日次勘定科目残高"""".update.apply()
    sql"""DELETE FROM "月次勘定科目残高"""".update.apply()
    sql"""DELETE FROM "勘定科目マスタ"""".update.apply()

  behavior of "日次勘定科目残高 CRUD操作"

  it should "日次残高レコードを登録できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      clearTestData()
      // テスト用勘定科目を登録
      insertTestAccount("1020", "普通預金", "資産")

      val entryDate = LocalDate.of(2025, 1, 15)
      val accountCode = "1020"

      // 日次残高を登録
      sql"""
        INSERT INTO "日次勘定科目残高" (
          "起票日", "勘定科目コード", "補助科目コード", "部門コード",
          "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
        ) VALUES (${entryDate}, ${accountCode}, '', '', '', 0, 100000.00, 0.00)
      """.update.apply()

      // データが正しく登録されている
      val result = sql"""
        SELECT * FROM "日次勘定科目残高"
        WHERE "起票日" = ${entryDate}
          AND "勘定科目コード" = ${accountCode}
      """.map(rs =>
        (BigDecimal(rs.bigDecimal("借方金額")), BigDecimal(rs.bigDecimal("貸方金額")))
      ).single.apply()

      result shouldBe defined
      val (debitAmount, creditAmount) = result.get
      debitAmount shouldBe BigDecimal("100000.00")
      creditAmount shouldBe BigDecimal("0.00")
    }
  }

  it should "複合主キーで一意性が保たれる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      clearTestData()
      insertTestAccount("1020", "普通預金", "資産")

      val entryDate = LocalDate.of(2025, 1, 15)
      val accountCode = "1020"

      sql"""
        INSERT INTO "日次勘定科目残高" (
          "起票日", "勘定科目コード", "補助科目コード", "部門コード",
          "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
        ) VALUES (${entryDate}, ${accountCode}, '', '', '', 0, 100000.00, 0.00)
      """.update.apply()

      // 同じキーで2回目の登録を試みるとエラー
      val exception = intercept[Exception] {
        sql"""
          INSERT INTO "日次勘定科目残高" (
            "起票日", "勘定科目コード", "補助科目コード", "部門コード",
            "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
          ) VALUES (${entryDate}, ${accountCode}, '', '', '', 0, 50000.00, 0.00)
        """.update.apply()
      }

      exception.getMessage should include("duplicate key value violates unique constraint")
    }
  }

  it should "部門別の残高を管理できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      clearTestData()
      insertTestAccount("4010", "売上高", "収益")

      val entryDate = LocalDate.of(2025, 1, 15)
      val accountCode = "4010"

      // 部門001と部門002の残高を登録
      sql"""
        INSERT INTO "日次勘定科目残高" (
          "起票日", "勘定科目コード", "補助科目コード", "部門コード",
          "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
        ) VALUES (${entryDate}, ${accountCode}, '', '001', '', 0, 0.00, 300000.00)
      """.update.apply()

      sql"""
        INSERT INTO "日次勘定科目残高" (
          "起票日", "勘定科目コード", "補助科目コード", "部門コード",
          "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
        ) VALUES (${entryDate}, ${accountCode}, '', '002', '', 0, 0.00, 200000.00)
      """.update.apply()

      // 部門別に集計できる
      val result = sql"""
        SELECT "部門コード", SUM("貸方金額") as 売上合計
        FROM "日次勘定科目残高"
        WHERE "勘定科目コード" = ${accountCode}
        GROUP BY "部門コード"
        ORDER BY "部門コード"
      """.map(rs => (rs.string("部門コード"), BigDecimal(rs.bigDecimal("売上合計")))).list.apply()

      result should have size 2
      result(0)._1 shouldBe "001"
      result(0)._2 shouldBe BigDecimal("300000.00")
      result(1)._1 shouldBe "002"
      result(1)._2 shouldBe BigDecimal("200000.00")
    }
  }

  it should "決算仕訳フラグで通常仕訳と決算仕訳を分けて管理できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      clearTestData()
      insertTestAccount("5110", "仕入", "費用")

      val entryDate = LocalDate.of(2025, 3, 31)
      val accountCode = "5110"

      // 通常仕訳と決算仕訳の残高を登録
      sql"""
        INSERT INTO "日次勘定科目残高" (
          "起票日", "勘定科目コード", "補助科目コード", "部門コード",
          "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
        ) VALUES (${entryDate}, ${accountCode}, '', '', '', 0, 1000000.00, 0.00)
      """.update.apply()

      sql"""
        INSERT INTO "日次勘定科目残高" (
          "起票日", "勘定科目コード", "補助科目コード", "部門コード",
          "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
        ) VALUES (${entryDate}, ${accountCode}, '', '', '', 1, 50000.00, 0.00)
      """.update.apply()

      // 通常仕訳のみの合計
      val normalTotal = sql"""
        SELECT SUM("借方金額")
        FROM "日次勘定科目残高"
        WHERE "勘定科目コード" = ${accountCode} AND "決算仕訳フラグ" = 0
      """.map(rs => Option(rs.bigDecimal(1)).map(BigDecimal(_))).single.apply().flatten

      // 決算仕訳のみの合計
      val settlementTotal = sql"""
        SELECT SUM("借方金額")
        FROM "日次勘定科目残高"
        WHERE "勘定科目コード" = ${accountCode} AND "決算仕訳フラグ" = 1
      """.map(rs => Option(rs.bigDecimal(1)).map(BigDecimal(_))).single.apply().flatten

      normalTotal shouldBe defined
      normalTotal.get shouldBe BigDecimal("1000000.00")
      settlementTotal shouldBe defined
      settlementTotal.get shouldBe BigDecimal("50000.00")
    }
  }

  behavior of "BalanceRepository"

  it should "UPSERTで日次残高を更新できる" in withContainers { container =>
    setupWithMigrations(container)
    val repository = new BalanceRepository()

    DB.localTx { implicit session =>
      clearTestData()
      insertTestAccount("1020", "普通預金", "資産")

      val entryDate = LocalDate.of(2025, 1, 15)
      val accountCode = "1020"

      // 1回目の登録
      repository.upsertDailyBalance(
        entryDate, accountCode, "", "", "", 0,
        BigDecimal("100000.00"), BigDecimal("0.00")
      )

      // 2回目の登録（同じキーで加算）
      repository.upsertDailyBalance(
        entryDate, accountCode, "", "", "", 0,
        BigDecimal("50000.00"), BigDecimal("0.00")
      )

      // 結果を確認
      val key = DailyAccountBalanceKey(entryDate, accountCode, "", "", "", 0)
      val result = repository.findDailyBalance(key)

      result shouldBe defined
      result.get.debitAmount shouldBe BigDecimal("150000.00")
      result.get.creditAmount shouldBe BigDecimal("0.00")
    }
  }

  it should "日付範囲で日次残高を検索できる" in withContainers { container =>
    setupWithMigrations(container)
    val repository = new BalanceRepository()

    DB.localTx { implicit session =>
      clearTestData()
      insertTestAccount("1020", "普通預金", "資産")

      // 複数日付のデータを登録
      repository.upsertDailyBalance(
        LocalDate.of(2025, 1, 10), "1020", "", "", "", 0,
        BigDecimal("100000.00"), BigDecimal("0.00")
      )
      repository.upsertDailyBalance(
        LocalDate.of(2025, 1, 15), "1020", "", "", "", 0,
        BigDecimal("200000.00"), BigDecimal("0.00")
      )
      repository.upsertDailyBalance(
        LocalDate.of(2025, 1, 20), "1020", "", "", "", 0,
        BigDecimal("300000.00"), BigDecimal("0.00")
      )

      // 日付範囲で検索
      val results = repository.findDailyBalancesByDateRange(
        LocalDate.of(2025, 1, 10),
        LocalDate.of(2025, 1, 15)
      )

      results should have size 2
      results.head.entryDate shouldBe LocalDate.of(2025, 1, 10)
      results.last.entryDate shouldBe LocalDate.of(2025, 1, 15)
    }
  }

  private def insertTestAccount(code: String, name: String, accountType: String)(implicit session: DBSession): Unit =
    sql"""
      INSERT INTO "勘定科目マスタ" (
        "勘定科目コード", "勘定科目名", "勘定科目種別", "残高"
      ) VALUES (
        ${code}, ${name}, ${accountType}::account_type, ${BigDecimal("0")}
      )
    """.update.apply()
