package com.example.accounting

import com.example.accounting.domain.balance.*
import com.example.accounting.infrastructure.out.persistence.balance.BalanceRepository
import com.example.db.DatabaseSpec
import org.scalatest.BeforeAndAfterEach
import scalikejdbc.*

import java.time.{LocalDate, LocalDateTime}

/**
 * 月次勘定科目残高テーブルのテスト
 */
class MonthlyAccountBalanceSpec extends DatabaseSpec with BeforeAndAfterEach:

  override def beforeEach(): Unit =
    super.beforeEach()

  private def clearTestData()(implicit session: DBSession): Unit =
    sql"""DELETE FROM "月次勘定科目残高"""".update.apply()
    sql"""DELETE FROM "日次勘定科目残高"""".update.apply()
    sql"""DELETE FROM "勘定科目マスタ"""".update.apply()

  behavior of "月次勘定科目残高 CRUD操作"

  it should "月次残高レコードを登録できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      clearTestData()
      insertTestAccount("1020", "普通預金", "資産")

      // 月次残高を登録
      sql"""
        INSERT INTO "月次勘定科目残高" (
          "決算期", "月度", "勘定科目コード", "補助科目コード",
          "部門コード", "プロジェクトコード", "決算仕訳フラグ",
          "月初残高", "借方金額", "貸方金額", "月末残高"
        ) VALUES (2025, 1, '1020', '', '', '', 0, 1000000.00, 500000.00, 300000.00, 1200000.00)
      """.update.apply()

      // データが正しく登録されている
      val result = sql"""
        SELECT * FROM "月次勘定科目残高"
        WHERE "決算期" = 2025 AND "月度" = 1 AND "勘定科目コード" = '1020'
      """.map { rs =>
        (
          BigDecimal(rs.bigDecimal("月初残高")),
          BigDecimal(rs.bigDecimal("借方金額")),
          BigDecimal(rs.bigDecimal("貸方金額")),
          BigDecimal(rs.bigDecimal("月末残高")),
        )
      }.single.apply()

      result shouldBe defined
      val (opening, debit, credit, closing) = result.get
      opening shouldBe BigDecimal("1000000.00")
      debit shouldBe BigDecimal("500000.00")
      credit shouldBe BigDecimal("300000.00")
      closing shouldBe BigDecimal("1200000.00")
    }
  }

  it should "月度0は無効である" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      clearTestData()
      insertTestAccount("1020", "普通預金", "資産")
    }

    val exception0 = intercept[Exception] {
      DB.localTx { implicit session =>
        sql"""
          INSERT INTO "月次勘定科目残高" (
            "決算期", "月度", "勘定科目コード", "補助科目コード",
            "部門コード", "プロジェクトコード", "決算仕訳フラグ",
            "月初残高", "借方金額", "貸方金額", "月末残高"
          ) VALUES (2025, 0, '1020', '', '', '', 0, 0, 0, 0, 0)
        """.update.apply()
      }
    }
    exception0.getMessage should include("check_月次残高_月度範囲")
  }

  it should "月度13は無効である" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      clearTestData()
      insertTestAccount("1020", "普通預金", "資産")
    }

    val exception13 = intercept[Exception] {
      DB.localTx { implicit session =>
        sql"""
          INSERT INTO "月次勘定科目残高" (
            "決算期", "月度", "勘定科目コード", "補助科目コード",
            "部門コード", "プロジェクトコード", "決算仕訳フラグ",
            "月初残高", "借方金額", "貸方金額", "月末残高"
          ) VALUES (2025, 13, '1020', '', '', '', 0, 0, 0, 0, 0)
        """.update.apply()
      }
    }
    exception13.getMessage should include("check_月次残高_月度範囲")
  }

  behavior of "BalanceRepository 月次残高"

  it should "月次残高を登録/更新できる" in withContainers { container =>
    setupWithMigrations(container)
    val repository = new BalanceRepository()

    DB.localTx { implicit session =>
      clearTestData()
      insertTestAccount("1020", "普通預金", "資産")

      val balance = MonthlyAccountBalance(
        fiscalYear = 2025,
        month = 1,
        accountCode = "1020",
        subAccountCode = "",
        departmentCode = "",
        projectCode = "",
        settlementFlag = 0,
        openingBalance = BigDecimal("1000000.00"),
        debitAmount = BigDecimal("500000.00"),
        creditAmount = BigDecimal("300000.00"),
        closingBalance = BigDecimal("1200000.00"),
        createdAt = LocalDateTime.now(),
        updatedAt = LocalDateTime.now(),
      )

      repository.upsertMonthlyBalance(balance)

      val key = MonthlyAccountBalanceKey(2025, 1, "1020", "", "", "", 0)
      val result = repository.findMonthlyBalance(key)

      result shouldBe defined
      result.get.openingBalance shouldBe BigDecimal("1000000.00")
      result.get.closingBalance shouldBe BigDecimal("1200000.00")
    }
  }

  it should "決算期で月次残高を検索できる" in withContainers { container =>
    setupWithMigrations(container)
    val repository = new BalanceRepository()

    DB.localTx { implicit session =>
      clearTestData()
      insertTestAccount("1020", "普通預金", "資産")

      // 複数月のデータを登録
      (1 to 3).foreach { month =>
        val balance = MonthlyAccountBalance(
          fiscalYear = 2025,
          month = month,
          accountCode = "1020",
          subAccountCode = "",
          departmentCode = "",
          projectCode = "",
          settlementFlag = 0,
          openingBalance = BigDecimal(month * 100000),
          debitAmount = BigDecimal(month * 50000),
          creditAmount = BigDecimal(month * 30000),
          closingBalance = BigDecimal(month * 120000),
          createdAt = LocalDateTime.now(),
          updatedAt = LocalDateTime.now(),
        )
        repository.upsertMonthlyBalance(balance)
      }

      val results = repository.findMonthlyBalancesByFiscalYear(2025)

      results should have size 3
      results.head.month shouldBe 1
      results.last.month shouldBe 3
    }
  }

  behavior of "月次締め処理"

  it should "日次残高から月次残高を生成できる" in withContainers { container =>
    setupWithMigrations(container)
    val repository = new BalanceRepository()

    DB.localTx { implicit session =>
      clearTestData()
      insertTestAccount("1020", "普通預金", "資産")

      // 2025年1月の日次残高を登録
      repository.upsertDailyBalance(
        LocalDate.of(2025, 1, 10), "1020", "", "", "", 0,
        BigDecimal("100000.00"), BigDecimal("0.00")
      )
      repository.upsertDailyBalance(
        LocalDate.of(2025, 1, 15), "1020", "", "", "", 0,
        BigDecimal("200000.00"), BigDecimal("50000.00")
      )
      repository.upsertDailyBalance(
        LocalDate.of(2025, 1, 20), "1020", "", "", "", 0,
        BigDecimal("300000.00"), BigDecimal("100000.00")
      )

      // 月次締め処理を実行
      val affected = repository.closeMonth(2025, 1)

      affected shouldBe 1

      // 月次残高を確認
      val key = MonthlyAccountBalanceKey(2025, 1, "1020", "", "", "", 0)
      val result = repository.findMonthlyBalance(key)

      result shouldBe defined
      result.get.debitAmount shouldBe BigDecimal("600000.00")  // 100000 + 200000 + 300000
      result.get.creditAmount shouldBe BigDecimal("150000.00") // 0 + 50000 + 100000
      result.get.closingBalance shouldBe BigDecimal("450000.00") // 600000 - 150000
    }
  }

  behavior of "MonthlyAccountBalance ドメインモデル"

  it should "月末残高を正しく計算できる" in {
    val balance = MonthlyAccountBalance(
      fiscalYear = 2025,
      month = 1,
      accountCode = "1020",
      subAccountCode = "",
      departmentCode = "",
      projectCode = "",
      settlementFlag = 0,
      openingBalance = BigDecimal("1000000.00"),
      debitAmount = BigDecimal("500000.00"),
      creditAmount = BigDecimal("300000.00"),
      closingBalance = BigDecimal("1200000.00"),
      createdAt = LocalDateTime.now(),
      updatedAt = LocalDateTime.now(),
    )

    balance.calculateClosingBalance shouldBe BigDecimal("1200000.00")
    balance.isBalanceValid shouldBe true
  }

  it should "残高計算が不正な場合を検出できる" in {
    val balance = MonthlyAccountBalance(
      fiscalYear = 2025,
      month = 1,
      accountCode = "1020",
      subAccountCode = "",
      departmentCode = "",
      projectCode = "",
      settlementFlag = 0,
      openingBalance = BigDecimal("1000000.00"),
      debitAmount = BigDecimal("500000.00"),
      creditAmount = BigDecimal("300000.00"),
      closingBalance = BigDecimal("9999999.00"), // 不正な値
      createdAt = LocalDateTime.now(),
      updatedAt = LocalDateTime.now(),
    )

    balance.calculateClosingBalance shouldBe BigDecimal("1200000.00")
    balance.isBalanceValid shouldBe false
  }

  private def insertTestAccount(code: String, name: String, accountType: String)(implicit session: DBSession): Unit =
    sql"""
      INSERT INTO "勘定科目マスタ" (
        "勘定科目コード", "勘定科目名", "勘定科目種別", "残高"
      ) VALUES (
        ${code}, ${name}, ${accountType}::account_type, ${BigDecimal("0")}
      )
    """.update.apply()
