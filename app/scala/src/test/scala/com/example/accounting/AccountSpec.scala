package com.example.accounting

import com.example.db.DatabaseSpec
import org.scalatest.BeforeAndAfterEach
import scalikejdbc.*

/**
 * 勘定科目マスタのテスト
 *
 * このテストでは、勘定科目マスタに対するCRUD操作を検証します。
 * DatabaseSpecを継承してTestcontainersを使用し、
 * 実際のPostgreSQLコンテナでテストを実行します。
 */
class AccountSpec extends DatabaseSpec with BeforeAndAfterEach:

  override def beforeEach(): Unit =
    super.beforeEach()

  behavior of "勘定科目マスタ CRUD操作"

  it should "勘定科目を登録できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      // 1. テストデータを作成
      val result = sql"""
        INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "残高")
        VALUES (${"AS1000"}, ${"現金"}, ${"資産"}::account_type, ${BigDecimal("50000.00")})
        RETURNING "勘定科目ID", "勘定科目コード", "勘定科目名", "勘定科目種別", "残高"
      """.map { rs =>
        (
          rs.int("勘定科目ID"),
          rs.string("勘定科目コード"),
          rs.string("勘定科目名"),
          rs.string("勘定科目種別"),
          BigDecimal(rs.bigDecimal("残高")),
        )
      }.single.apply()

      // 2. 取得したデータが期待通りか検証
      result should not be None
      val (id, code, name, accountType, balance) = result.get
      code shouldBe "AS1000"
      name shouldBe "現金"
      accountType shouldBe "資産"
      balance shouldBe BigDecimal("50000.00")
    }
  }

  it should "すべての勘定科目を取得できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      // 1. 複数の勘定科目を登録
      insertAccount("ASALL100", "現金", "資産", "50000.00")
      insertAccount("ASALL200", "買掛金", "負債", "30000.00")
      insertAccount("ASALL300", "資本金", "純資産", "100000.00")

      // 2. すべての勘定科目を取得
      val codes = sql"""
        SELECT "勘定科目コード", "勘定科目名", "勘定科目種別", "残高"
        FROM "勘定科目マスタ"
        WHERE "勘定科目コード" LIKE 'ASALL%'
        ORDER BY "勘定科目コード"
      """.map(rs => rs.string("勘定科目コード")).list.apply()

      // 3. 期待通りのデータが取得できるか検証
      codes should have size 3
      codes shouldBe List("ASALL100", "ASALL200", "ASALL300")
    }
  }

  it should "勘定科目コードで検索できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      // 1. テストデータを登録
      insertAccount("AS3000", "現金", "資産", "50000.00")

      // 2. コードで検索
      val result = sql"""
        SELECT "勘定科目コード", "勘定科目名", "勘定科目種別"
        FROM "勘定科目マスタ"
        WHERE "勘定科目コード" = ${"AS3000"}
      """.map { rs =>
        (rs.string("勘定科目コード"), rs.string("勘定科目名"), rs.string("勘定科目種別"))
      }.single.apply()

      // 3. 正しいデータが取得できるか検証
      result should not be None
      val (code, name, accountType) = result.get
      code shouldBe "AS3000"
      name shouldBe "現金"
      accountType shouldBe "資産"
    }
  }

  it should "勘定科目を更新できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      // 1. データを登録
      val accountId = insertAccount("AS4000", "現金", "資産", "50000.00")

      // 2. データを更新
      val updated = sql"""
        UPDATE "勘定科目マスタ"
        SET "勘定科目名" = ${"現金及び預金"}, "残高" = ${BigDecimal("75000.00")}, "更新日時" = CURRENT_TIMESTAMP
        WHERE "勘定科目ID" = ${accountId}
      """.update.apply()

      updated shouldBe 1

      // 3. 更新されたか検証
      val result = sql"""
        SELECT "勘定科目コード", "勘定科目名", "残高"
        FROM "勘定科目マスタ"
        WHERE "勘定科目ID" = ${accountId}
      """.map { rs =>
        (rs.string("勘定科目コード"), rs.string("勘定科目名"), BigDecimal(rs.bigDecimal("残高")))
      }.single.apply()

      result should not be None
      val (code, name, balance) = result.get
      name shouldBe "現金及び預金"
      balance shouldBe BigDecimal("75000.00")
      code shouldBe "AS4000" // 変更していない項目は保持される
    }
  }

  it should "勘定科目を削除できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      // 1. データを登録
      val accountId = insertAccount("AS5000", "現金", "資産", "50000.00")

      // 2. データを削除
      val deleted = sql"""
        DELETE FROM "勘定科目マスタ"
        WHERE "勘定科目ID" = ${accountId}
      """.update.apply()

      deleted shouldBe 1

      // 3. データが削除されたか検証
      val count = sql"""
        SELECT COUNT(*) as count
        FROM "勘定科目マスタ"
        WHERE "勘定科目ID" = ${accountId}
      """.map(_.int("count")).single.apply()

      count shouldBe Some(0)
    }
  }

  behavior of "勘定科目マスタ 検索操作"

  it should "勘定科目種別でフィルタリングできる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      // 1. 複数の勘定科目を登録
      insertAccount("ASFLT100", "現金", "資産", "50000.00")
      insertAccount("ASFLT200", "買掛金", "負債", "30000.00")
      insertAccount("ASFLT300", "資本金", "純資産", "100000.00")

      // 2. 資産勘定のみを取得（ASFLTで始まるものだけをフィルタリング）
      val assetNames = sql"""
        SELECT "勘定科目名"
        FROM "勘定科目マスタ"
        WHERE "勘定科目種別" = ${"資産"}::account_type AND "勘定科目コード" LIKE 'ASFLT%'
      """.map(_.string("勘定科目名")).list.apply()

      // 3. 正しくフィルタリングされるか検証
      assetNames should have size 1
      assetNames shouldBe List("現金")

      // 4. 負債勘定のみを取得
      val liabilityNames = sql"""
        SELECT "勘定科目名"
        FROM "勘定科目マスタ"
        WHERE "勘定科目種別" = ${"負債"}::account_type AND "勘定科目コード" LIKE 'ASFLT%'
      """.map(_.string("勘定科目名")).list.apply()

      liabilityNames should have size 1
      liabilityNames shouldBe List("買掛金")
    }
  }

  /**
   * 勘定科目を登録するヘルパーメソッド
   */
  private def insertAccount(
      code: String,
      name: String,
      accountType: String,
      balance: String,
  )(implicit session: DBSession): Int =
    sql"""
      INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "残高")
      VALUES (${code}, ${name}, ${accountType}::account_type, ${BigDecimal(balance)})
      RETURNING "勘定科目ID"
    """.map(_.int("勘定科目ID")).single.apply().getOrElse {
      throw new RuntimeException("勘定科目の登録に失敗しました")
    }
