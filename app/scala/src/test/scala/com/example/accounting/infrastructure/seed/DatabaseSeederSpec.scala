package com.example.accounting.infrastructure.seed

import com.example.db.DatabaseSpec
import org.scalatest.BeforeAndAfterEach
import scalikejdbc.*

/**
 * DatabaseSeeder のテスト
 *
 * Testcontainers を使用して実際のデータベースでシードデータの投入をテストします。
 */
class DatabaseSeederSpec extends DatabaseSpec with BeforeAndAfterEach:

  override def beforeEach(): Unit =
    super.beforeEach()

  behavior of "DatabaseSeeder"

  it should "シードデータを投入できる" in withContainers { container =>
    setupWithMigrations(container)

    // シードデータ投入
    DatabaseSeeder.seedDatabase()

    DB.readOnly { implicit session =>
      // 勘定科目マスタの確認
      val accountCount =
        sql"""SELECT COUNT(*) FROM "勘定科目マスタ"""".map(_.int(1)).single.apply().getOrElse(0)
      accountCount shouldBe AccountingSeedData.accounts.size

      // 勘定科目構成マスタの確認
      val structureCount =
        sql"""SELECT COUNT(*) FROM "勘定科目構成マスタ"""".map(_.int(1)).single.apply().getOrElse(0)
      structureCount shouldBe AccountingSeedData.accountStructures.size

      // 仕訳の確認
      val journalCount = sql"""SELECT COUNT(*) FROM "仕訳"""".map(_.int(1)).single.apply().getOrElse(0)
      journalCount shouldBe (AccountingSeedData.fy2021Journals.size + AccountingSeedData.fy2022Journals.size)

      // 日次残高の確認
      val dailyBalanceCount =
        sql"""SELECT COUNT(*) FROM "日次勘定科目残高"""".map(_.int(1)).single.apply().getOrElse(0)
      dailyBalanceCount shouldBe (AccountingSeedData.fy2021DailyBalances.size + AccountingSeedData.fy2022DailyBalances.size)

      // 月次残高の確認
      val monthlyBalanceCount =
        sql"""SELECT COUNT(*) FROM "月次勘定科目残高"""".map(_.int(1)).single.apply().getOrElse(0)
      monthlyBalanceCount shouldBe (AccountingSeedData.fy2021MonthlyBalances.size + AccountingSeedData.fy2022MonthlyBalances.size)
    }
  }

  it should "再実行時に既存データをクリーンアップしてから投入する" in withContainers { container =>
    setupWithMigrations(container)

    // 1回目の投入
    DatabaseSeeder.seedDatabase()

    // 2回目の投入
    DatabaseSeeder.seedDatabase()

    DB.readOnly { implicit session =>
      // 勘定科目マスタの確認（重複なく同じ件数であること）
      val accountCount =
        sql"""SELECT COUNT(*) FROM "勘定科目マスタ"""".map(_.int(1)).single.apply().getOrElse(0)
      accountCount shouldBe AccountingSeedData.accounts.size
    }
  }

  behavior of "勘定科目マスタのシードデータ"

  it should "資産科目が正しく投入される" in withContainers { container =>
    setupWithMigrations(container)
    DatabaseSeeder.seedDatabase()

    DB.readOnly { implicit session =>
      val assets = sql"""
        SELECT * FROM "勘定科目マスタ"
        WHERE "勘定科目種別" = '資産'::account_type
        ORDER BY "勘定科目コード"
      """.map(rs => (rs.string("勘定科目コード"), rs.string("勘定科目名"))).list.apply()

      assets should not be empty
      assets.map(_._1) should contain("1")
      assets.map(_._1) should contain("11")
      assets.map(_._1) should contain("111")
    }
  }

  it should "負債科目が正しく投入される" in withContainers { container =>
    setupWithMigrations(container)
    DatabaseSeeder.seedDatabase()

    DB.readOnly { implicit session =>
      val liabilities = sql"""
        SELECT * FROM "勘定科目マスタ"
        WHERE "勘定科目種別" = '負債'::account_type
        ORDER BY "勘定科目コード"
      """.map(rs => (rs.string("勘定科目コード"), rs.string("勘定科目名"))).list.apply()

      liabilities should not be empty
      liabilities.map(_._1) should contain("2")
    }
  }

  it should "収益科目が正しく投入される" in withContainers { container =>
    setupWithMigrations(container)
    DatabaseSeeder.seedDatabase()

    DB.readOnly { implicit session =>
      val revenues = sql"""
        SELECT * FROM "勘定科目マスタ"
        WHERE "勘定科目種別" = '収益'::account_type
        ORDER BY "勘定科目コード"
      """.map(rs => (rs.string("勘定科目コード"), rs.string("勘定科目名"))).list.apply()

      revenues should not be empty
      revenues.map(_._1) should contain("4")
    }
  }

  it should "費用科目が正しく投入される" in withContainers { container =>
    setupWithMigrations(container)
    DatabaseSeeder.seedDatabase()

    DB.readOnly { implicit session =>
      val expenses = sql"""
        SELECT * FROM "勘定科目マスタ"
        WHERE "勘定科目種別" = '費用'::account_type
        ORDER BY "勘定科目コード"
      """.map(rs => (rs.string("勘定科目コード"), rs.string("勘定科目名"))).list.apply()

      expenses should not be empty
      expenses.map(_._1) should contain("5")
    }
  }

  behavior of "勘定科目構成マスタのシードデータ"

  it should "階層構造が正しく投入される" in withContainers { container =>
    setupWithMigrations(container)
    DatabaseSeeder.seedDatabase()

    DB.readOnly { implicit session =>
      val structures = sql"""
        SELECT "勘定科目コード", "チルダパス", "階層レベル"
        FROM "勘定科目構成マスタ"
        ORDER BY "チルダパス"
      """.map(rs =>
        (rs.string("勘定科目コード"), rs.string("チルダパス"), rs.int("階層レベル"))
      ).list.apply()

      structures should not be empty

      // ルートノードの確認
      val rootNodes = structures.filter(_._3 == 1)
      rootNodes should not be empty

      // 子ノードの確認
      val childNodes = structures.filter(_._3 > 1)
      childNodes should not be empty
    }
  }

  behavior of "仕訳データのシードデータ"

  it should "令和3年度の仕訳が正しく投入される" in withContainers { container =>
    setupWithMigrations(container)
    DatabaseSeeder.seedDatabase()

    DB.readOnly { implicit session =>
      val fy2021Journals = sql"""
        SELECT * FROM "仕訳"
        WHERE "仕訳伝票番号" LIKE 'J2021-%'
      """.map(_.string("仕訳伝票番号")).list.apply()

      fy2021Journals.size shouldBe AccountingSeedData.fy2021Journals.size
    }
  }

  it should "令和4年度の仕訳が正しく投入される" in withContainers { container =>
    setupWithMigrations(container)
    DatabaseSeeder.seedDatabase()

    DB.readOnly { implicit session =>
      val fy2022Journals = sql"""
        SELECT * FROM "仕訳"
        WHERE "仕訳伝票番号" LIKE 'J2022-%'
      """.map(_.string("仕訳伝票番号")).list.apply()

      fy2022Journals.size shouldBe AccountingSeedData.fy2022Journals.size
    }
  }

  behavior of "日次残高データのシードデータ"

  it should "令和3年度の日次残高が正しく投入される" in withContainers { container =>
    setupWithMigrations(container)
    DatabaseSeeder.seedDatabase()

    DB.readOnly { implicit session =>
      val fy2021DailyBalances = sql"""
        SELECT * FROM "日次勘定科目残高"
        WHERE "起票日" BETWEEN '2021-04-01' AND '2022-03-31'
      """.map(rs =>
        (rs.string("勘定科目コード"), rs.bigDecimal("借方金額"), rs.bigDecimal("貸方金額"))
      ).list.apply()

      fy2021DailyBalances.size shouldBe AccountingSeedData.fy2021DailyBalances.size
    }
  }

  it should "令和4年度の日次残高が正しく投入される" in withContainers { container =>
    setupWithMigrations(container)
    DatabaseSeeder.seedDatabase()

    DB.readOnly { implicit session =>
      val fy2022DailyBalances = sql"""
        SELECT * FROM "日次勘定科目残高"
        WHERE "起票日" BETWEEN '2022-04-01' AND '2023-03-31'
      """.map(rs =>
        (rs.string("勘定科目コード"), rs.bigDecimal("借方金額"), rs.bigDecimal("貸方金額"))
      ).list.apply()

      fy2022DailyBalances.size shouldBe AccountingSeedData.fy2022DailyBalances.size
    }
  }

  behavior of "月次残高データのシードデータ"

  it should "令和3年度の月次残高が正しく投入される" in withContainers { container =>
    setupWithMigrations(container)
    DatabaseSeeder.seedDatabase()

    DB.readOnly { implicit session =>
      val fy2021MonthlyBalances = sql"""
        SELECT * FROM "月次勘定科目残高"
        WHERE "決算期" = 2021
      """.map(rs =>
        (
          rs.string("勘定科目コード"),
          rs.int("月度"),
          rs.bigDecimal("月初残高"),
          rs.bigDecimal("月末残高"),
        )
      ).list.apply()

      fy2021MonthlyBalances.size shouldBe AccountingSeedData.fy2021MonthlyBalances.size
    }
  }

  it should "令和4年度の月次残高が正しく投入される" in withContainers { container =>
    setupWithMigrations(container)
    DatabaseSeeder.seedDatabase()

    DB.readOnly { implicit session =>
      val fy2022MonthlyBalances = sql"""
        SELECT * FROM "月次勘定科目残高"
        WHERE "決算期" = 2022
      """.map(rs =>
        (
          rs.string("勘定科目コード"),
          rs.int("月度"),
          rs.bigDecimal("月初残高"),
          rs.bigDecimal("月末残高"),
        )
      ).list.apply()

      fy2022MonthlyBalances.size shouldBe AccountingSeedData.fy2022MonthlyBalances.size
    }
  }

  behavior of "データの整合性"

  it should "仕訳貸借明細に借方と貸方の両方が存在する" in withContainers { container =>
    setupWithMigrations(container)
    DatabaseSeeder.seedDatabase()

    DB.readOnly { implicit session =>
      // 借方と貸方それぞれの件数を取得
      val debitCount =
        sql"""SELECT COUNT(*) FROM "仕訳貸借明細" WHERE "仕訳行貸借区分" = 'D'""".map(_.int(1)).single.apply().getOrElse(0)
      val creditCount =
        sql"""SELECT COUNT(*) FROM "仕訳貸借明細" WHERE "仕訳行貸借区分" = 'C'""".map(_.int(1)).single.apply().getOrElse(0)

      // 借方と貸方の両方が存在すること
      debitCount should be > 0
      creditCount should be > 0

      // 全体の明細数が正しいこと
      val totalEntries = AccountingSeedData.fy2021Entries.size + AccountingSeedData.fy2022Entries.size
      (debitCount + creditCount) shouldBe totalEntries
    }
  }

  it should "外部キー制約が満たされている" in withContainers { container =>
    setupWithMigrations(container)
    DatabaseSeeder.seedDatabase()

    DB.readOnly { implicit session =>
      // 仕訳貸借明細の勘定科目コードが勘定科目マスタに存在すること
      val orphanedEntries = sql"""
        SELECT dc.*
        FROM "仕訳貸借明細" dc
        LEFT JOIN "勘定科目マスタ" a ON dc."勘定科目コード" = a."勘定科目コード"
        WHERE a."勘定科目コード" IS NULL
      """.map(_.string("勘定科目コード")).list.apply()

      orphanedEntries shouldBe empty

      // 日次残高の勘定科目コードが勘定科目マスタに存在すること
      val orphanedDailyBalances = sql"""
        SELECT d.*
        FROM "日次勘定科目残高" d
        LEFT JOIN "勘定科目マスタ" a ON d."勘定科目コード" = a."勘定科目コード"
        WHERE a."勘定科目コード" IS NULL
      """.map(_.string("勘定科目コード")).list.apply()

      orphanedDailyBalances shouldBe empty

      // 月次残高の勘定科目コードが勘定科目マスタに存在すること
      val orphanedMonthlyBalances = sql"""
        SELECT m.*
        FROM "月次勘定科目残高" m
        LEFT JOIN "勘定科目マスタ" a ON m."勘定科目コード" = a."勘定科目コード"
        WHERE a."勘定科目コード" IS NULL
      """.map(_.string("勘定科目コード")).list.apply()

      orphanedMonthlyBalances shouldBe empty
    }
  }
