package com.example.financial.seed

import cats.effect.*
import cats.implicits.*
import scalikejdbc.*
import com.example.financial.config.DatabaseConfig
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import java.time.LocalDate

/**
 * データベースシーダー
 *
 * サンプルデータを投入します。
 */
object DatabaseSeeder extends IOApp:

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  override def run(args: List[String]): IO[ExitCode] =
    val dbConfig = DatabaseConfig.loadFromEnv()

    DatabaseConfig.initialize(dbConfig).use { _ =>
      for
        _ <- logger.info("Starting database seeding...")
        _ <- DatabaseConfig.migrate(dbConfig)
        _ <- seedAccounts
        _ <- seedJournals
        _ <- logger.info("Database seeding completed!")
      yield ExitCode.Success
    }

  /**
   * 勘定科目のシードデータ
   */
  private def seedAccounts: IO[Unit] = IO {
    DB.localTx { implicit session =>
      // 既存データをクリア
      sql"DELETE FROM journal_entries".update.apply()
      sql"DELETE FROM journals".update.apply()
      sql"DELETE FROM account_balances".update.apply()
      sql"DELETE FROM accounts".update.apply()

      // 勘定科目を登録
      val accounts = List(
        // 流動資産
        ("1100", "現金", "asset", 1),
        ("1110", "当座預金", "asset", 2),
        ("1120", "普通預金", "asset", 3),
        ("1200", "売掛金", "asset", 4),
        ("1300", "商品", "asset", 5),
        // 固定資産
        ("1210", "建物", "asset", 10),
        ("1220", "備品", "asset", 11),
        ("1230", "車両運搬具", "asset", 12),
        // 流動負債
        ("2100", "買掛金", "liability", 20),
        ("2110", "未払金", "liability", 21),
        ("2120", "短期借入金", "liability", 22),
        // 固定負債
        ("2200", "長期借入金", "liability", 30),
        // 純資産
        ("3100", "資本金", "equity", 40),
        ("3200", "繰越利益剰余金", "equity", 41),
        // 売上高
        ("4100", "売上高", "revenue", 50),
        ("4200", "受取利息", "revenue", 51),
        // 売上原価
        ("5100", "売上原価", "expense", 60),
        ("5110", "仕入高", "expense", 61),
        // 販売費及び一般管理費
        ("5200", "給料手当", "expense", 70),
        ("5210", "広告宣伝費", "expense", 71),
        ("5220", "通信費", "expense", 72),
        ("5230", "水道光熱費", "expense", 73),
        ("5240", "減価償却費", "expense", 74),
        ("5250", "消耗品費", "expense", 75),
        // 営業外費用
        ("5300", "支払利息", "expense", 80)
      )

      accounts.foreach { case (code, name, accountType, order) =>
        sql"""
          INSERT INTO accounts (
            account_code, account_name, account_type,
            is_summary_account, display_order, is_aggregation_target, balance
          ) VALUES (
            $code, $name, $accountType, false, $order, true, 0
          )
        """.update.apply()
      }

      println(s"Seeded ${accounts.size} accounts")
    }
  }

  /**
   * 仕訳のシードデータ（2024年度）
   */
  private def seedJournals: IO[Unit] = IO {
    DB.localTx { implicit session =>
      val fiscalYear = 2024

      // サンプル仕訳データ
      val journalData = List(
        // 1月: 資本金の払い込み
        (LocalDate.of(2024, 1, 5), "資本金払込", List(
          ("1120", BigDecimal(10000000), BigDecimal(0)),  // 普通預金
          ("3100", BigDecimal(0), BigDecimal(10000000))   // 資本金
        )),
        // 1月: 商品仕入
        (LocalDate.of(2024, 1, 10), "商品仕入（掛け）", List(
          ("5110", BigDecimal(3000000), BigDecimal(0)),   // 仕入高
          ("2100", BigDecimal(0), BigDecimal(3000000))    // 買掛金
        )),
        // 1月: 売上
        (LocalDate.of(2024, 1, 15), "商品売上（掛け）", List(
          ("1200", BigDecimal(5000000), BigDecimal(0)),   // 売掛金
          ("4100", BigDecimal(0), BigDecimal(5000000))    // 売上高
        )),
        // 1月: 売上原価計上
        (LocalDate.of(2024, 1, 15), "売上原価計上", List(
          ("5100", BigDecimal(2000000), BigDecimal(0)),   // 売上原価
          ("1300", BigDecimal(0), BigDecimal(2000000))    // 商品
        )),
        // 2月: 買掛金支払
        (LocalDate.of(2024, 2, 10), "買掛金支払", List(
          ("2100", BigDecimal(3000000), BigDecimal(0)),   // 買掛金
          ("1120", BigDecimal(0), BigDecimal(3000000))    // 普通預金
        )),
        // 2月: 売掛金回収
        (LocalDate.of(2024, 2, 20), "売掛金回収", List(
          ("1120", BigDecimal(5000000), BigDecimal(0)),   // 普通預金
          ("1200", BigDecimal(0), BigDecimal(5000000))    // 売掛金
        )),
        // 3月: 給与支払
        (LocalDate.of(2024, 3, 25), "3月分給与", List(
          ("5200", BigDecimal(800000), BigDecimal(0)),    // 給料手当
          ("1120", BigDecimal(0), BigDecimal(800000))     // 普通預金
        )),
        // 3月: 水道光熱費
        (LocalDate.of(2024, 3, 31), "3月分水道光熱費", List(
          ("5230", BigDecimal(50000), BigDecimal(0)),     // 水道光熱費
          ("1120", BigDecimal(0), BigDecimal(50000))      // 普通預金
        )),
        // 4月: 追加商品仕入
        (LocalDate.of(2024, 4, 5), "商品仕入（掛け）", List(
          ("5110", BigDecimal(4000000), BigDecimal(0)),   // 仕入高
          ("2100", BigDecimal(0), BigDecimal(4000000))    // 買掛金
        )),
        // 4月: 売上
        (LocalDate.of(2024, 4, 20), "商品売上（掛け）", List(
          ("1200", BigDecimal(8000000), BigDecimal(0)),   // 売掛金
          ("4100", BigDecimal(0), BigDecimal(8000000))    // 売上高
        )),
        // 4月: 売上原価
        (LocalDate.of(2024, 4, 20), "売上原価計上", List(
          ("5100", BigDecimal(3500000), BigDecimal(0)),   // 売上原価
          ("1300", BigDecimal(0), BigDecimal(3500000))    // 商品
        ))
      )

      var journalCount = 0
      journalData.foreach { case (date, description, entries) =>
        // 仕訳ヘッダー
        val journalId = sql"""
          INSERT INTO journals (journal_date, description, fiscal_year)
          VALUES ($date, $description, $fiscalYear)
        """.updateAndReturnGeneratedKey.apply()

        // 仕訳明細
        entries.foreach { case (accountCode, debit, credit) =>
          sql"""
            INSERT INTO journal_entries (journal_id, account_code, debit_amount, credit_amount, description)
            VALUES ($journalId, $accountCode, $debit, $credit, '')
          """.update.apply()
        }

        journalCount += 1
      }

      println(s"Seeded $journalCount journals for fiscal year $fiscalYear")
    }
  }
