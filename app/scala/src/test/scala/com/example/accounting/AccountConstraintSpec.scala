package com.example.accounting

import com.example.db.DatabaseSpec
import org.scalatest.BeforeAndAfterEach
import scalikejdbc.*
import org.postgresql.util.PSQLException

/**
 * 勘定科目マスタの制約テスト
 */
class AccountConstraintSpec extends DatabaseSpec with BeforeAndAfterEach:

  override def beforeEach(): Unit =
    super.beforeEach()

  behavior of "勘定科目マスタ 制約チェック"

  it should "BSPL区分は 'B' または 'P' のみ許可" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      // 不正な値を挿入しようとすると例外が発生する
      val exception = intercept[PSQLException] {
        sql"""
          INSERT INTO "勘定科目マスタ"
          ("勘定科目コード", "勘定科目名", "勘定科目種別", "BSPL区分", "残高")
          VALUES (${"AC1000"}, ${"現金"}, ${"資産"}::account_type, ${"X"}, ${BigDecimal("0")})
        """.update.apply()
      }

      exception.getMessage should include("check constraint")
    }
  }

  it should "BSPL区分 'B' は正常に登録できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      val result = sql"""
        INSERT INTO "勘定科目マスタ"
        ("勘定科目コード", "勘定科目名", "勘定科目種別", "BSPL区分", "残高")
        VALUES (${"AC2000"}, ${"現金"}, ${"資産"}::account_type, ${"B"}, ${BigDecimal("0")})
        RETURNING "BSPL区分"
      """.map(_.string("BSPL区分")).single.apply()

      result shouldBe Some("B")
    }
  }

  it should "BSPL区分 'P' は正常に登録できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      val result = sql"""
        INSERT INTO "勘定科目マスタ"
        ("勘定科目コード", "勘定科目名", "勘定科目種別", "BSPL区分", "残高")
        VALUES (${"AC3000"}, ${"売上高"}, ${"収益"}::account_type, ${"P"}, ${BigDecimal("0")})
        RETURNING "BSPL区分"
      """.map(_.string("BSPL区分")).single.apply()

      result shouldBe Some("P")
    }
  }

  it should "取引要素区分は 1-5 のみ許可" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      val exception = intercept[PSQLException] {
        sql"""
          INSERT INTO "勘定科目マスタ"
          ("勘定科目コード", "勘定科目名", "勘定科目種別", "取引要素区分", "残高")
          VALUES (${"AC4000"}, ${"現金"}, ${"資産"}::account_type, ${"9"}, ${BigDecimal("0")})
        """.update.apply()
      }

      exception.getMessage should include("check constraint")
    }
  }

  it should "費用区分は 1-3 のみ許可" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      val exception = intercept[PSQLException] {
        sql"""
          INSERT INTO "勘定科目マスタ"
          ("勘定科目コード", "勘定科目名", "勘定科目種別", "費用区分", "残高")
          VALUES (${"AC5000"}, ${"売上原価"}, ${"費用"}::account_type, ${"9"}, ${BigDecimal("0")})
        """.update.apply()
      }

      exception.getMessage should include("check constraint")
    }
  }
