package com.example.accounting

import com.example.db.DatabaseSpec
import org.scalatest.BeforeAndAfterEach
import scalikejdbc.*

import java.time.LocalDate

/**
 * 3層構造仕訳テーブルのテスト
 *
 * 仕訳（ヘッダー） + 仕訳明細 + 仕訳貸借明細の3層構造を検証します。
 */
class Journal3LayerSpec extends DatabaseSpec with BeforeAndAfterEach:

  override def beforeEach(): Unit =
    super.beforeEach()

  behavior of "3層構造仕訳テーブル"

  it should "単純な仕訳を登録できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      // テスト用勘定科目を登録
      insertTestAccounts("J3L1")

      // Given: 現金100,000円で商品を仕入れる仕訳
      val journalNo = "J3L1-20250101-001"
      val journalDate = LocalDate.of(2025, 1, 1)
      val inputDate = LocalDate.of(2025, 1, 1)

      // When: 仕訳を登録
      // 1. 仕訳ヘッダー
      sql"""
        INSERT INTO "仕訳" (
          "仕訳伝票番号", "起票日", "入力日", "決算仕訳フラグ", "単振フラグ",
          "仕訳伝票区分", "定期計上フラグ", "赤伝フラグ"
        ) VALUES (${journalNo}, ${journalDate}, ${inputDate}, 0, 1, 0, 0, 0)
      """.update.apply()

      // 2. 仕訳明細（1行）
      sql"""
        INSERT INTO "仕訳明細" (
          "仕訳伝票番号", "仕訳行番号", "行摘要"
        ) VALUES (${journalNo}, 1, ${"商品仕入"})
      """.update.apply()

      // 3. 仕訳貸借明細（借方：仕入、貸方：現金）
      sql"""
        INSERT INTO "仕訳貸借明細" (
          "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
          "通貨コード", "為替レート", "勘定科目コード",
          "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
        ) VALUES (${journalNo}, 1, ${"D"}, ${"JPY"}, ${BigDecimal("1.00")}, ${"J3L1-5110"},
                  ${BigDecimal("100000.00")}, ${BigDecimal("100000.00")}, 0)
      """.update.apply()

      sql"""
        INSERT INTO "仕訳貸借明細" (
          "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
          "通貨コード", "為替レート", "勘定科目コード",
          "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
        ) VALUES (${journalNo}, 1, ${"C"}, ${"JPY"}, ${BigDecimal("1.00")}, ${"J3L1-1010"},
                  ${BigDecimal("100000.00")}, ${BigDecimal("100000.00")}, 0)
      """.update.apply()

      // Then: データが正しく登録されていることを確認
      // 1. 仕訳が登録されている
      val journalCount = sql"""
        SELECT COUNT(*) as count FROM "仕訳" WHERE "仕訳伝票番号" = ${journalNo}
      """.map(_.int("count")).single.apply()
      journalCount shouldBe Some(1)

      // 2. 仕訳明細が登録されている
      val detailCount = sql"""
        SELECT COUNT(*) as count FROM "仕訳明細" WHERE "仕訳伝票番号" = ${journalNo}
      """.map(_.int("count")).single.apply()
      detailCount shouldBe Some(1)

      // 3. 仕訳貸借明細が2件（借方・貸方）登録されている
      val itemCount = sql"""
        SELECT COUNT(*) as count FROM "仕訳貸借明細" WHERE "仕訳伝票番号" = ${journalNo}
      """.map(_.int("count")).single.apply()
      itemCount shouldBe Some(2)

      // 4. 借方・貸方の合計が一致する（複式簿記の原理）
      val balance = sql"""
        SELECT
          SUM(CASE WHEN "仕訳行貸借区分" = 'D' THEN "仕訳金額" ELSE 0 END) as debit_total,
          SUM(CASE WHEN "仕訳行貸借区分" = 'C' THEN "仕訳金額" ELSE 0 END) as credit_total
        FROM "仕訳貸借明細"
        WHERE "仕訳伝票番号" = ${journalNo}
      """.map { rs =>
        (BigDecimal(rs.bigDecimal("debit_total")), BigDecimal(rs.bigDecimal("credit_total")))
      }.single.apply()

      balance should not be None
      val (debitTotal, creditTotal) = balance.get
      debitTotal shouldBe creditTotal
      debitTotal shouldBe BigDecimal("100000.00")
    }
  }

  it should "複合仕訳を登録できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      // テスト用勘定科目を登録
      insertTestAccounts("J3L2")

      // Given: 売掛金の回収（振込手数料差引）
      val journalNo = "J3L2-20250102-001"
      val journalDate = LocalDate.of(2025, 1, 2)
      val inputDate = LocalDate.of(2025, 1, 2)

      // When: 仕訳を登録
      // 1. 仕訳ヘッダー（単振フラグ=0: 複合仕訳）
      sql"""
        INSERT INTO "仕訳" (
          "仕訳伝票番号", "起票日", "入力日", "決算仕訳フラグ", "単振フラグ",
          "仕訳伝票区分", "定期計上フラグ", "赤伝フラグ"
        ) VALUES (${journalNo}, ${journalDate}, ${inputDate}, 0, 0, 0, 0, 0)
      """.update.apply()

      // 2. 仕訳明細（2行）
      sql"""
        INSERT INTO "仕訳明細" ("仕訳伝票番号", "仕訳行番号", "行摘要")
        VALUES (${journalNo}, 1, ${"売掛金回収（A社）"})
      """.update.apply()

      sql"""
        INSERT INTO "仕訳明細" ("仕訳伝票番号", "仕訳行番号", "行摘要")
        VALUES (${journalNo}, 2, ${"振込手数料"})
      """.update.apply()

      // 3. 仕訳貸借明細
      // 行1-借方: 普通預金 99,560円
      sql"""
        INSERT INTO "仕訳貸借明細" (
          "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
          "通貨コード", "為替レート", "勘定科目コード",
          "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
        ) VALUES (${journalNo}, 1, ${"D"}, ${"JPY"}, ${BigDecimal("1.00")}, ${"J3L2-1020"},
                  ${BigDecimal("99560.00")}, ${BigDecimal("99560.00")}, 1)
      """.update.apply()

      // 行1-貸方: 売掛金 99,560円
      sql"""
        INSERT INTO "仕訳貸借明細" (
          "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
          "通貨コード", "為替レート", "勘定科目コード",
          "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
        ) VALUES (${journalNo}, 1, ${"C"}, ${"JPY"}, ${BigDecimal("1.00")}, ${"J3L2-1130"},
                  ${BigDecimal("99560.00")}, ${BigDecimal("99560.00")}, 0)
      """.update.apply()

      // 行2-借方: 支払手数料 440円
      sql"""
        INSERT INTO "仕訳貸借明細" (
          "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
          "通貨コード", "為替レート", "勘定科目コード",
          "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
        ) VALUES (${journalNo}, 2, ${"D"}, ${"JPY"}, ${BigDecimal("1.00")}, ${"J3L2-5410"},
                  ${BigDecimal("440.00")}, ${BigDecimal("440.00")}, 0)
      """.update.apply()

      // 行2-貸方: 売掛金 440円
      sql"""
        INSERT INTO "仕訳貸借明細" (
          "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
          "通貨コード", "為替レート", "勘定科目コード",
          "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
        ) VALUES (${journalNo}, 2, ${"C"}, ${"JPY"}, ${BigDecimal("1.00")}, ${"J3L2-1130"},
                  ${BigDecimal("440.00")}, ${BigDecimal("440.00")}, 0)
      """.update.apply()

      // Then: データが正しく登録されていることを確認
      // 1. 仕訳明細が2件登録されている
      val detailCount = sql"""
        SELECT COUNT(*) as count FROM "仕訳明細" WHERE "仕訳伝票番号" = ${journalNo}
      """.map(_.int("count")).single.apply()
      detailCount shouldBe Some(2)

      // 2. 仕訳貸借明細が4件登録されている
      val itemCount = sql"""
        SELECT COUNT(*) as count FROM "仕訳貸借明細" WHERE "仕訳伝票番号" = ${journalNo}
      """.map(_.int("count")).single.apply()
      itemCount shouldBe Some(4)

      // 3. 借方・貸方の合計が一致する
      val balance = sql"""
        SELECT
          SUM(CASE WHEN "仕訳行貸借区分" = 'D' THEN "仕訳金額" ELSE 0 END) as debit_total,
          SUM(CASE WHEN "仕訳行貸借区分" = 'C' THEN "仕訳金額" ELSE 0 END) as credit_total
        FROM "仕訳貸借明細"
        WHERE "仕訳伝票番号" = ${journalNo}
      """.map { rs =>
        (BigDecimal(rs.bigDecimal("debit_total")), BigDecimal(rs.bigDecimal("credit_total")))
      }.single.apply()

      balance should not be None
      val (debitTotal, creditTotal) = balance.get
      debitTotal shouldBe creditTotal
      debitTotal shouldBe BigDecimal("100000.00")

      // 4. 単振フラグが0（複合仕訳）になっている
      val tanpuFlag = sql"""
        SELECT "単振フラグ" FROM "仕訳" WHERE "仕訳伝票番号" = ${journalNo}
      """.map(_.int("単振フラグ")).single.apply()
      tanpuFlag shouldBe Some(0)
    }
  }

  it should "仕訳削除時に明細も削除される" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      // テスト用勘定科目を登録
      insertTestAccounts("J3L3")

      // Given: 仕訳を登録
      val journalNo = "J3L3-20250103-001"
      val journalDate = LocalDate.of(2025, 1, 3)

      sql"""
        INSERT INTO "仕訳" (
          "仕訳伝票番号", "起票日", "入力日", "決算仕訳フラグ", "単振フラグ",
          "仕訳伝票区分", "定期計上フラグ", "赤伝フラグ"
        ) VALUES (${journalNo}, ${journalDate}, ${journalDate}, 0, 1, 0, 0, 0)
      """.update.apply()

      sql"""
        INSERT INTO "仕訳明細" ("仕訳伝票番号", "仕訳行番号", "行摘要")
        VALUES (${journalNo}, 1, ${"テスト"})
      """.update.apply()

      sql"""
        INSERT INTO "仕訳貸借明細" (
          "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
          "通貨コード", "為替レート", "勘定科目コード",
          "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
        ) VALUES (${journalNo}, 1, ${"D"}, ${"JPY"}, ${BigDecimal("1.00")}, ${"J3L3-1010"},
                  ${BigDecimal("10000.00")}, ${BigDecimal("10000.00")}, 0)
      """.update.apply()

      // When: 仕訳を削除
      sql"""DELETE FROM "仕訳" WHERE "仕訳伝票番号" = ${journalNo}""".update.apply()

      // Then: 明細と貸借明細も自動削除される（CASCADE）
      val detailCount = sql"""
        SELECT COUNT(*) as count FROM "仕訳明細" WHERE "仕訳伝票番号" = ${journalNo}
      """.map(_.int("count")).single.apply()
      detailCount shouldBe Some(0)

      val itemCount = sql"""
        SELECT COUNT(*) as count FROM "仕訳貸借明細" WHERE "仕訳伝票番号" = ${journalNo}
      """.map(_.int("count")).single.apply()
      itemCount shouldBe Some(0)
    }
  }

  it should "赤伝票には赤黒伝票番号が必須" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      val journalNo = "J3L4-20250104-001"
      val journalDate = LocalDate.of(2025, 1, 4)

      // 赤伝フラグ=1で赤黒伝票番号なしは制約違反
      val exception = intercept[Exception] {
        sql"""
          INSERT INTO "仕訳" (
            "仕訳伝票番号", "起票日", "入力日", "決算仕訳フラグ", "単振フラグ",
            "仕訳伝票区分", "定期計上フラグ", "赤伝フラグ", "赤黒伝票番号"
          ) VALUES (${journalNo}, ${journalDate}, ${journalDate}, 0, 1, 0, 0, 1, NULL)
        """.update.apply()
      }

      exception.getMessage should include("check")
    }
  }

  it should "赤伝票は赤黒伝票番号付きで登録できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      val originalNo = "J3L5-20250105-001"
      val redSlipNo = "J3L5-20250105-002"
      val journalDate = LocalDate.of(2025, 1, 5)

      // 元の仕訳を登録
      sql"""
        INSERT INTO "仕訳" (
          "仕訳伝票番号", "起票日", "入力日", "決算仕訳フラグ", "単振フラグ",
          "仕訳伝票区分", "定期計上フラグ", "赤伝フラグ"
        ) VALUES (${originalNo}, ${journalDate}, ${journalDate}, 0, 1, 0, 0, 0)
      """.update.apply()

      // 赤伝票を登録（赤黒伝票番号あり）
      sql"""
        INSERT INTO "仕訳" (
          "仕訳伝票番号", "起票日", "入力日", "決算仕訳フラグ", "単振フラグ",
          "仕訳伝票区分", "定期計上フラグ", "赤伝フラグ", "赤黒伝票番号"
        ) VALUES (${redSlipNo}, ${journalDate}, ${journalDate}, 0, 1, 0, 0, 1, ${originalNo})
      """.update.apply()

      // 赤伝票が正しく登録されたか確認
      val result = sql"""
        SELECT "赤伝フラグ", "赤黒伝票番号" FROM "仕訳" WHERE "仕訳伝票番号" = ${redSlipNo}
      """.map { rs =>
        (rs.int("赤伝フラグ"), rs.string("赤黒伝票番号"))
      }.single.apply()

      result should not be None
      val (redFlag, redBlackNo) = result.get
      redFlag shouldBe 1
      redBlackNo shouldBe originalNo
    }
  }

  it should "起票日と入力日を分離して管理できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      val journalNo = "J3L6-20250110-001"
      // 遡及入力：12/25の取引を1/10に入力
      val journalDate = LocalDate.of(2024, 12, 25)
      val inputDate = LocalDate.of(2025, 1, 10)

      sql"""
        INSERT INTO "仕訳" (
          "仕訳伝票番号", "起票日", "入力日", "決算仕訳フラグ", "単振フラグ",
          "仕訳伝票区分", "定期計上フラグ", "赤伝フラグ"
        ) VALUES (${journalNo}, ${journalDate}, ${inputDate}, 0, 1, 0, 0, 0)
      """.update.apply()

      val result = sql"""
        SELECT "起票日", "入力日" FROM "仕訳" WHERE "仕訳伝票番号" = ${journalNo}
      """.map { rs =>
        (rs.localDate("起票日"), rs.localDate("入力日"))
      }.single.apply()

      result should not be None
      val (savedJournalDate, savedInputDate) = result.get
      savedJournalDate shouldBe journalDate
      savedInputDate shouldBe inputDate
    }
  }

  it should "決算仕訳フラグで通常仕訳と決算仕訳を区別できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      val normalNo = "J3L7-20250115-001"
      val settlementNo = "J3L7-20250115-002"
      val journalDate = LocalDate.of(2025, 1, 15)

      // 通常仕訳
      sql"""
        INSERT INTO "仕訳" (
          "仕訳伝票番号", "起票日", "入力日", "決算仕訳フラグ", "単振フラグ",
          "仕訳伝票区分", "定期計上フラグ", "赤伝フラグ"
        ) VALUES (${normalNo}, ${journalDate}, ${journalDate}, 0, 1, 0, 0, 0)
      """.update.apply()

      // 決算仕訳
      sql"""
        INSERT INTO "仕訳" (
          "仕訳伝票番号", "起票日", "入力日", "決算仕訳フラグ", "単振フラグ",
          "仕訳伝票区分", "定期計上フラグ", "赤伝フラグ"
        ) VALUES (${settlementNo}, ${journalDate}, ${journalDate}, 1, 1, 0, 0, 0)
      """.update.apply()

      // 決算仕訳のみ取得
      val settlementJournals = sql"""
        SELECT "仕訳伝票番号" FROM "仕訳"
        WHERE "仕訳伝票番号" LIKE 'J3L7%' AND "決算仕訳フラグ" = 1
      """.map(_.string("仕訳伝票番号")).list.apply()

      settlementJournals should have size 1
      settlementJournals.head shouldBe settlementNo
    }
  }

  /**
   * テスト用勘定科目を登録するヘルパーメソッド
   */
  private def insertTestAccounts(prefix: String)(implicit session: DBSession): Unit =
    val accounts = List(
      (s"$prefix-1010", "現金", "資産"),
      (s"$prefix-1020", "普通預金", "資産"),
      (s"$prefix-1130", "売掛金", "資産"),
      (s"$prefix-5110", "仕入", "費用"),
      (s"$prefix-5410", "支払手数料", "費用"),
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
