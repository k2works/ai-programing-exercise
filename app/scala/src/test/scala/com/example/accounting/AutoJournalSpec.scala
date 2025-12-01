package com.example.accounting

import com.example.db.DatabaseSpec
import org.scalatest.BeforeAndAfterEach
import scalikejdbc.*

import java.time.LocalDateTime

/**
 * 自動仕訳テーブルのテスト
 *
 * 自動仕訳管理、パターン、パターン明細、実行ログの4テーブルを検証します。
 */
class AutoJournalSpec extends DatabaseSpec with BeforeAndAfterEach:

  override def beforeEach(): Unit =
    super.beforeEach()

  behavior of "自動仕訳管理"

  it should "自動仕訳管理を登録できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      val managementId = "AJM001"

      sql"""
        INSERT INTO "自動仕訳管理" (
          "自動仕訳管理ID", "自動仕訳名", "ソーステーブル名", "有効フラグ"
        ) VALUES (
          ${managementId}, ${"売上データ自動仕訳"}, ${"売上データ"}, 1
        )
      """.update.apply()

      val result = sql"""
        SELECT "自動仕訳管理ID", "自動仕訳名", "ソーステーブル名", "有効フラグ"
        FROM "自動仕訳管理"
        WHERE "自動仕訳管理ID" = ${managementId}
      """.map { rs =>
        (
          rs.string("自動仕訳管理ID"),
          rs.string("自動仕訳名"),
          rs.string("ソーステーブル名"),
          rs.int("有効フラグ"),
        )
      }.single.apply()

      result should not be None
      val (id, name, source, enabled) = result.get
      id shouldBe managementId
      name shouldBe "売上データ自動仕訳"
      source shouldBe "売上データ"
      enabled shouldBe 1
    }
  }

  it should "最終処理日時を更新できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      val managementId = "AJM002"
      val lastProcessed = LocalDateTime.of(2025, 1, 15, 10, 30, 0)

      sql"""
        INSERT INTO "自動仕訳管理" (
          "自動仕訳管理ID", "自動仕訳名", "ソーステーブル名"
        ) VALUES (
          ${managementId}, ${"給与データ自動仕訳"}, ${"給与データ"}
        )
      """.update.apply()

      // 最終処理日時を更新
      sql"""
        UPDATE "自動仕訳管理"
        SET "最終処理日時" = ${lastProcessed}
        WHERE "自動仕訳管理ID" = ${managementId}
      """.update.apply()

      val result = sql"""
        SELECT "最終処理日時" FROM "自動仕訳管理"
        WHERE "自動仕訳管理ID" = ${managementId}
      """.map(_.localDateTime("最終処理日時")).single.apply()

      result shouldBe Some(lastProcessed)
    }
  }

  behavior of "自動仕訳パターン"

  it should "パターンを登録できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      // 事前に管理レコードを登録
      sql"""
        INSERT INTO "自動仕訳管理" (
          "自動仕訳管理ID", "自動仕訳名", "ソーステーブル名"
        ) VALUES (${"AJM-P1"}, ${"売上自動仕訳"}, ${"売上データ"})
      """.update.apply()

      val patternId = "PAT001"

      sql"""
        INSERT INTO "自動仕訳パターン" (
          "パターンID", "自動仕訳管理ID", "パターン名", "条件式", "優先順位"
        ) VALUES (
          ${patternId}, ${"AJM-P1"}, ${"通常売上パターン"}, ${"category = 'NORMAL'"}, 1
        )
      """.update.apply()

      val result = sql"""
        SELECT "パターンID", "パターン名", "条件式", "優先順位"
        FROM "自動仕訳パターン"
        WHERE "パターンID" = ${patternId}
      """.map { rs =>
        (
          rs.string("パターンID"),
          rs.string("パターン名"),
          rs.string("条件式"),
          rs.int("優先順位"),
        )
      }.single.apply()

      result should not be None
      val (id, name, condition, priority) = result.get
      id shouldBe patternId
      name shouldBe "通常売上パターン"
      condition shouldBe "category = 'NORMAL'"
      priority shouldBe 1
    }
  }

  it should "管理削除時にパターンも削除される" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      val managementId = "AJM-P2"
      val patternId = "PAT002"

      sql"""
        INSERT INTO "自動仕訳管理" (
          "自動仕訳管理ID", "自動仕訳名", "ソーステーブル名"
        ) VALUES (${managementId}, ${"テスト自動仕訳"}, ${"テストデータ"})
      """.update.apply()

      sql"""
        INSERT INTO "自動仕訳パターン" (
          "パターンID", "自動仕訳管理ID", "パターン名"
        ) VALUES (${patternId}, ${managementId}, ${"テストパターン"})
      """.update.apply()

      // 管理を削除
      sql"""DELETE FROM "自動仕訳管理" WHERE "自動仕訳管理ID" = ${managementId}""".update.apply()

      // パターンも削除されているか確認
      val count = sql"""
        SELECT COUNT(*) as count FROM "自動仕訳パターン"
        WHERE "パターンID" = ${patternId}
      """.map(_.int("count")).single.apply()

      count shouldBe Some(0)
    }
  }

  behavior of "自動仕訳パターン明細"

  it should "パターン明細を登録できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      // テスト用勘定科目を登録
      insertTestAccounts("AJ")

      // 事前データ登録
      sql"""
        INSERT INTO "自動仕訳管理" (
          "自動仕訳管理ID", "自動仕訳名", "ソーステーブル名"
        ) VALUES (${"AJM-D1"}, ${"売上自動仕訳"}, ${"売上データ"})
      """.update.apply()

      sql"""
        INSERT INTO "自動仕訳パターン" (
          "パターンID", "自動仕訳管理ID", "パターン名"
        ) VALUES (${"PAT-D1"}, ${"AJM-D1"}, ${"売上パターン"})
      """.update.apply()

      // パターン明細を登録
      // 借方：売掛金
      sql"""
        INSERT INTO "自動仕訳パターン明細" (
          "パターンID", "行番号", "貸借区分", "勘定科目コード", "金額式", "摘要テンプレート"
        ) VALUES (${"PAT-D1"}, 1, ${"D"}, ${"AJ-1130"}, ${"amount * 1.1"}, ${"売上計上：{customer_name}"})
      """.update.apply()

      // 貸方：売上
      sql"""
        INSERT INTO "自動仕訳パターン明細" (
          "パターンID", "行番号", "貸借区分", "勘定科目コード", "金額式", "摘要テンプレート"
        ) VALUES (${"PAT-D1"}, 2, ${"C"}, ${"AJ-4100"}, ${"amount"}, ${"売上：{customer_name}"})
      """.update.apply()

      // 貸方：仮受消費税
      sql"""
        INSERT INTO "自動仕訳パターン明細" (
          "パターンID", "行番号", "貸借区分", "勘定科目コード", "金額式", "摘要テンプレート"
        ) VALUES (${"PAT-D1"}, 3, ${"C"}, ${"AJ-2120"}, ${"amount * 0.1"}, ${"消費税：{customer_name}"})
      """.update.apply()

      val details = sql"""
        SELECT "行番号", "貸借区分", "勘定科目コード", "金額式"
        FROM "自動仕訳パターン明細"
        WHERE "パターンID" = ${"PAT-D1"}
        ORDER BY "行番号"
      """.map { rs =>
        (rs.int("行番号"), rs.string("貸借区分"), rs.string("勘定科目コード"), rs.string("金額式"))
      }.list.apply()

      details should have size 3
      details(0) shouldBe (1, "D", "AJ-1130", "amount * 1.1")
      details(1) shouldBe (2, "C", "AJ-4100", "amount")
      details(2) shouldBe (3, "C", "AJ-2120", "amount * 0.1")
    }
  }

  behavior of "自動仕訳実行ログ"

  it should "実行ログを記録できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      val managementId = "AJM-L1"
      val startTime = LocalDateTime.of(2025, 1, 15, 10, 0, 0)
      val endTime = LocalDateTime.of(2025, 1, 15, 10, 5, 0)

      sql"""
        INSERT INTO "自動仕訳管理" (
          "自動仕訳管理ID", "自動仕訳名", "ソーステーブル名"
        ) VALUES (${managementId}, ${"売上自動仕訳"}, ${"売上データ"})
      """.update.apply()

      // 実行ログを記録
      val logId = sql"""
        INSERT INTO "自動仕訳実行ログ" (
          "自動仕訳管理ID", "実行開始日時", "実行終了日時",
          "処理件数", "成功件数", "エラー件数", "実行結果"
        ) VALUES (
          ${managementId}, ${startTime}, ${endTime},
          100, 98, 2, ${"PARTIAL"}
        )
        RETURNING "実行ログID"
      """.map(_.long("実行ログID")).single.apply()

      logId should not be None

      val result = sql"""
        SELECT "処理件数", "成功件数", "エラー件数", "実行結果"
        FROM "自動仕訳実行ログ"
        WHERE "実行ログID" = ${logId.get}
      """.map { rs =>
        (rs.int("処理件数"), rs.int("成功件数"), rs.int("エラー件数"), rs.string("実行結果"))
      }.single.apply()

      result should not be None
      val (total, success, error, status) = result.get
      total shouldBe 100
      success shouldBe 98
      error shouldBe 2
      status shouldBe "PARTIAL"
    }
  }

  it should "実行結果は指定された値のみ許可" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      sql"""
        INSERT INTO "自動仕訳管理" (
          "自動仕訳管理ID", "自動仕訳名", "ソーステーブル名"
        ) VALUES (${"AJM-L2"}, ${"テスト"}, ${"テストデータ"})
      """.update.apply()

      // 不正な実行結果は制約違反
      val exception = intercept[Exception] {
        sql"""
          INSERT INTO "自動仕訳実行ログ" (
            "自動仕訳管理ID", "実行開始日時", "実行結果"
          ) VALUES (${"AJM-L2"}, ${LocalDateTime.now()}, ${"INVALID"})
        """.update.apply()
      }

      exception.getMessage should include("check")
    }
  }

  it should "有効な実行結果ステータスを登録できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      sql"""
        INSERT INTO "自動仕訳管理" (
          "自動仕訳管理ID", "自動仕訳名", "ソーステーブル名"
        ) VALUES (${"AJM-L3"}, ${"ステータステスト"}, ${"テストデータ"})
      """.update.apply()

      val validStatuses = List("RUNNING", "SUCCESS", "FAILED", "PARTIAL")

      validStatuses.foreach { status =>
        sql"""
          INSERT INTO "自動仕訳実行ログ" (
            "自動仕訳管理ID", "実行開始日時", "実行結果"
          ) VALUES (${"AJM-L3"}, ${LocalDateTime.now()}, ${status})
        """.update.apply()
      }

      val count = sql"""
        SELECT COUNT(*) as count FROM "自動仕訳実行ログ"
        WHERE "自動仕訳管理ID" = ${"AJM-L3"}
      """.map(_.int("count")).single.apply()

      count shouldBe Some(4)
    }
  }

  /**
   * テスト用勘定科目を登録するヘルパーメソッド
   */
  private def insertTestAccounts(prefix: String)(implicit session: DBSession): Unit =
    val accounts = List(
      (s"$prefix-1130", "売掛金", "資産"),
      (s"$prefix-2120", "仮受消費税", "負債"),
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
