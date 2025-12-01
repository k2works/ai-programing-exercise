package com.example.accounting

import com.example.db.DatabaseSpec
import org.scalatest.BeforeAndAfterEach
import scalikejdbc.*

/**
 * 勘定科目マスタのリファクタリングテスト
 */
class AccountRefactoringSpec extends DatabaseSpec with BeforeAndAfterEach:

  override def beforeEach(): Unit =
    super.beforeEach()

  behavior of "勘定科目マスタ リファクタリング"

  it should "BSPL区分を設定できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      val result = sql"""
        INSERT INTO "勘定科目マスタ"
        ("勘定科目コード", "勘定科目名", "勘定科目種別", "BSPL区分", "残高")
        VALUES (${"RF1000"}, ${"現金"}, ${"資産"}::account_type, ${"B"}, ${BigDecimal("0")})
        RETURNING "勘定科目コード", "BSPL区分"
      """.map { rs =>
        (rs.string("勘定科目コード"), rs.string("BSPL区分"))
      }.single.apply()

      result should not be None
      val (code, bspl) = result.get
      code shouldBe "RF1000"
      bspl shouldBe "B"
    }
  }

  it should "取引要素区分を設定できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      val result = sql"""
        INSERT INTO "勘定科目マスタ"
        ("勘定科目コード", "勘定科目名", "勘定科目種別", "BSPL区分", "取引要素区分", "残高")
        VALUES (${"RF2000"}, ${"現金"}, ${"資産"}::account_type, ${"B"}, ${"1"}, ${BigDecimal("0")})
        RETURNING "勘定科目コード", "取引要素区分"
      """.map { rs =>
        (rs.string("勘定科目コード"), rs.string("取引要素区分"))
      }.single.apply()

      result should not be None
      val (code, element) = result.get
      code shouldBe "RF2000"
      element shouldBe "1" // 1: 資産
    }
  }

  it should "集計科目を設定できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      val result = sql"""
        INSERT INTO "勘定科目マスタ"
        ("勘定科目コード", "勘定科目名", "勘定科目種別", "合計科目", "残高")
        VALUES (${"RF3000"}, ${"流動資産"}, ${"資産"}::account_type, ${true}, ${BigDecimal("0")})
        RETURNING "勘定科目コード", "合計科目"
      """.map { rs =>
        (rs.string("勘定科目コード"), rs.boolean("合計科目"))
      }.single.apply()

      result should not be None
      val (code, isSum) = result.get
      code shouldBe "RF3000"
      isSum shouldBe true
    }
  }
