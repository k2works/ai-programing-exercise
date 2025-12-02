package com.example.accounting.infrastructure.seed

import scalikejdbc.*
import com.example.db.DatabaseConfig

/**
 * データベースに Seed データを投入するオブジェクト
 *
 * 実行方法:
 *   sbt seed
 *   または
 *   sbt "runMain com.example.accounting.infrastructure.seed.DatabaseSeeder"
 */
object DatabaseSeeder:

  def main(args: Array[String]): Unit =
    // データベース接続の初期化
    DatabaseConfig.setup()

    try
      seedDatabase()
      println("Seed data insertion completed successfully!")
    catch
      case e: Exception =>
        println(s"Seed data insertion failed: ${e.getMessage}")
        e.printStackTrace()
    finally
      DatabaseConfig.close()

  /**
   * Seed データを投入する
   */
  def seedDatabase(): Unit =
    DB.localTx { implicit session =>
      println("Seeding database...")

      // 既存データのクリーンアップ(外部キー制約を考慮した順序で削除)
      cleanupExistingData()

      // マスタデータの投入
      seedAccounts()
      seedAccountStructures()

      // トランザクションデータの投入
      seedFY2021Data()
      seedFY2022Data()

      println("Database seeding completed successfully!")
    }

  /**
   * 既存データのクリーンアップ
   * 外部キー制約を考慮した順序で削除
   */
  private def cleanupExistingData()(implicit session: DBSession): Unit =
    println("Cleaning up existing data...")

    // 月次残高データ削除
    sql"""DELETE FROM "月次勘定科目残高"""".update.apply()
    // 日次残高データ削除
    sql"""DELETE FROM "日次勘定科目残高"""".update.apply()
    // 仕訳明細データ削除
    sql"""DELETE FROM "仕訳貸借明細"""".update.apply()
    sql"""DELETE FROM "仕訳明細"""".update.apply()
    // 仕訳データ削除
    sql"""DELETE FROM "仕訳"""".update.apply()
    // 勘定科目構成削除
    sql"""DELETE FROM "勘定科目構成マスタ"""".update.apply()
    // 勘定科目削除
    sql"""DELETE FROM "勘定科目マスタ"""".update.apply()

    println("Existing data cleaned up")

  /**
   * 勘定科目マスタデータを投入
   */
  private def seedAccounts()(implicit session: DBSession): Unit =
    println("Seeding accounts...")

    AccountingSeedData.accounts.foreach { account =>
      sql"""
        INSERT INTO "勘定科目マスタ" (
          "勘定科目コード",
          "勘定科目名",
          "勘定科目種別",
          "残高"
        ) VALUES (${account.accountCode}, ${account.accountName}, ${account.accountType}::account_type, ${BigDecimal(0)})
      """.update.apply()
    }

    println(s"Seeded ${AccountingSeedData.accounts.size} accounts")

  /**
   * 勘定科目構成マスタデータを投入
   */
  private def seedAccountStructures()(implicit session: DBSession): Unit =
    println("Seeding account structures...")

    AccountingSeedData.accountStructures.foreach { structure =>
      sql"""
        INSERT INTO "勘定科目構成マスタ" (
          "勘定科目コード",
          "チルダパス",
          "階層レベル"
        ) VALUES (${structure.accountCode}, ${structure.accountPath}, ${structure.accountPath.count(_ == '~') + 1})
      """.update.apply()
    }

    println(s"Seeded ${AccountingSeedData.accountStructures.size} account structures")

  /**
   * 令和3年度のデータを投入
   */
  private def seedFY2021Data()(implicit session: DBSession): Unit =
    println("Seeding FY2021 data...")

    // 仕訳投入
    AccountingSeedData.fy2021Journals.zipWithIndex.foreach { case (journal, idx) =>
      val journalNo = f"J2021-${idx + 1}%03d"
      sql"""
        INSERT INTO "仕訳" ("仕訳伝票番号", "起票日", "入力日")
        VALUES ($journalNo, ${journal.journalDate}, ${journal.journalDate})
      """.update.apply()

      // 仕訳明細と貸借明細投入
      AccountingSeedData.fy2021Entries.zipWithIndex.foreach { case (entry, lineIdx) =>
        val lineNo = lineIdx + 1
        sql"""
          INSERT INTO "仕訳明細" (
            "仕訳伝票番号",
            "仕訳行番号",
            "行摘要"
          ) VALUES ($journalNo, $lineNo, ${entry.description})
        """.update.apply()

        // 借方または貸方を判定
        val dcType = if entry.debitAmount > 0 then "D" else "C"
        val amount = if entry.debitAmount > 0 then entry.debitAmount else entry.creditAmount
        sql"""
          INSERT INTO "仕訳貸借明細" (
            "仕訳伝票番号",
            "仕訳行番号",
            "仕訳行貸借区分",
            "勘定科目コード",
            "仕訳金額",
            "基軸換算仕訳金額"
          ) VALUES ($journalNo, $lineNo, $dcType, ${entry.accountCode}, $amount, $amount)
        """.update.apply()
      }
    }

    println(s"Seeded ${AccountingSeedData.fy2021Entries.size} FY2021 journal entries")

    // 日次残高投入
    AccountingSeedData.fy2021DailyBalances.foreach { balance =>
      sql"""
        INSERT INTO "日次勘定科目残高" (
          "起票日",
          "勘定科目コード",
          "補助科目コード",
          "部門コード",
          "プロジェクトコード",
          "決算仕訳フラグ",
          "借方金額",
          "貸方金額"
        ) VALUES (${balance.balanceDate}, ${balance.accountCode}, '', '', '', 0, ${balance.debitAmount}, ${balance.creditAmount})
      """.update.apply()
    }

    println(s"Seeded ${AccountingSeedData.fy2021DailyBalances.size} FY2021 daily balances")

    // 月次残高投入
    AccountingSeedData.fy2021MonthlyBalances.foreach { balance =>
      sql"""
        INSERT INTO "月次勘定科目残高" (
          "決算期",
          "月度",
          "勘定科目コード",
          "補助科目コード",
          "部門コード",
          "プロジェクトコード",
          "決算仕訳フラグ",
          "月初残高",
          "借方金額",
          "貸方金額",
          "月末残高"
        ) VALUES (${balance.fiscalYear}, ${balance.month}, ${balance.accountCode}, '', '', '', 0, ${balance.beginningBalance}, ${balance.debitAmount}, ${balance.creditAmount}, ${balance.endingBalance})
      """.update.apply()
    }

    println(s"Seeded ${AccountingSeedData.fy2021MonthlyBalances.size} FY2021 monthly balances")

  /**
   * 令和4年度のデータを投入
   */
  private def seedFY2022Data()(implicit session: DBSession): Unit =
    println("Seeding FY2022 data...")

    // 仕訳投入
    AccountingSeedData.fy2022Journals.zipWithIndex.foreach { case (journal, idx) =>
      val journalNo = f"J2022-${idx + 1}%03d"
      sql"""
        INSERT INTO "仕訳" ("仕訳伝票番号", "起票日", "入力日")
        VALUES ($journalNo, ${journal.journalDate}, ${journal.journalDate})
      """.update.apply()

      // 仕訳明細と貸借明細投入
      AccountingSeedData.fy2022Entries.zipWithIndex.foreach { case (entry, lineIdx) =>
        val lineNo = lineIdx + 1
        sql"""
          INSERT INTO "仕訳明細" (
            "仕訳伝票番号",
            "仕訳行番号",
            "行摘要"
          ) VALUES ($journalNo, $lineNo, ${entry.description})
        """.update.apply()

        // 借方または貸方を判定
        val dcType = if entry.debitAmount > 0 then "D" else "C"
        val amount = if entry.debitAmount > 0 then entry.debitAmount else entry.creditAmount
        sql"""
          INSERT INTO "仕訳貸借明細" (
            "仕訳伝票番号",
            "仕訳行番号",
            "仕訳行貸借区分",
            "勘定科目コード",
            "仕訳金額",
            "基軸換算仕訳金額"
          ) VALUES ($journalNo, $lineNo, $dcType, ${entry.accountCode}, $amount, $amount)
        """.update.apply()
      }
    }

    println(s"Seeded ${AccountingSeedData.fy2022Entries.size} FY2022 journal entries")

    // 日次残高投入
    AccountingSeedData.fy2022DailyBalances.foreach { balance =>
      sql"""
        INSERT INTO "日次勘定科目残高" (
          "起票日",
          "勘定科目コード",
          "補助科目コード",
          "部門コード",
          "プロジェクトコード",
          "決算仕訳フラグ",
          "借方金額",
          "貸方金額"
        ) VALUES (${balance.balanceDate}, ${balance.accountCode}, '', '', '', 0, ${balance.debitAmount}, ${balance.creditAmount})
      """.update.apply()
    }

    println(s"Seeded ${AccountingSeedData.fy2022DailyBalances.size} FY2022 daily balances")

    // 月次残高投入
    AccountingSeedData.fy2022MonthlyBalances.foreach { balance =>
      sql"""
        INSERT INTO "月次勘定科目残高" (
          "決算期",
          "月度",
          "勘定科目コード",
          "補助科目コード",
          "部門コード",
          "プロジェクトコード",
          "決算仕訳フラグ",
          "月初残高",
          "借方金額",
          "貸方金額",
          "月末残高"
        ) VALUES (${balance.fiscalYear}, ${balance.month}, ${balance.accountCode}, '', '', '', 0, ${balance.beginningBalance}, ${balance.debitAmount}, ${balance.creditAmount}, ${balance.endingBalance})
      """.update.apply()
    }

    println(s"Seeded ${AccountingSeedData.fy2022MonthlyBalances.size} FY2022 monthly balances")
