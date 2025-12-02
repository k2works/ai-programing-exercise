package com.example.accounting.api

import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport.*
import com.example.accounting.application.service.FinancialStatementService
import com.example.accounting.infrastructure.http.FinancialStatementRoutes
import com.example.accounting.infrastructure.http.dto.*
import com.example.accounting.infrastructure.http.JsonFormats.{*, given}
import com.example.db.DatabaseSpec
import org.scalatest.BeforeAndAfterEach
import scalikejdbc.*

import java.time.LocalDate

/**
 * 財務諸表 API のテスト
 */
class FinancialStatementRoutesSpec extends DatabaseSpec with ScalatestRouteTest with BeforeAndAfterEach:

  private var routes: FinancialStatementRoutes = _

  override def beforeEach(): Unit =
    super.beforeEach()

  override def afterAll(): Unit =
    super.afterAll()
    cleanUp()

  private def setupRoutes(container: com.dimafeng.testcontainers.PostgreSQLContainer): Unit =
    setupWithMigrations(container)
    val service = FinancialStatementService()
    routes = FinancialStatementRoutes(service)

  private def clearTestData()(implicit session: DBSession): Unit =
    sql"""DELETE FROM "日次勘定科目残高"""".update.apply()
    sql"""DELETE FROM "月次勘定科目残高"""".update.apply()
    sql"""DELETE FROM "勘定科目マスタ"""".update.apply()

  private def setupBalanceSheetTestData()(implicit session: DBSession): Unit =
    sql"""
      INSERT INTO "勘定科目マスタ" (
        "勘定科目コード", "勘定科目名", "勘定科目種別", "BSPL区分", "残高"
      ) VALUES
      ('1110', '普通預金', '資産'::account_type, 'B', 0),
      ('1410', '建物', '資産'::account_type, 'B', 0),
      ('2110', '買掛金', '負債'::account_type, 'B', 0),
      ('2510', '長期借入金', '負債'::account_type, 'B', 0),
      ('3110', '資本金', '純資産'::account_type, 'B', 0)
    """.update.apply()

    val asOfDate = LocalDate.of(2024, 1, 31)
    sql"""
      INSERT INTO "日次勘定科目残高" (
        "起票日", "勘定科目コード", "補助科目コード", "部門コード",
        "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
      ) VALUES
      (${asOfDate}, '1110', '', '', '', 0, 8000000, 0),
      (${asOfDate}, '1410', '', '', '', 0, 2000000, 0),
      (${asOfDate}, '2110', '', '', '', 0, 0, 500000),
      (${asOfDate}, '2510', '', '', '', 0, 0, 4500000),
      (${asOfDate}, '3110', '', '', '', 0, 0, 5000000)
    """.update.apply()

  private def setupIncomeStatementTestData()(implicit session: DBSession): Unit =
    sql"""
      INSERT INTO "勘定科目マスタ" (
        "勘定科目コード", "勘定科目名", "勘定科目種別", "BSPL区分", "残高"
      ) VALUES
      ('4110', '売上高', '収益'::account_type, 'P', 0),
      ('5110', '売上原価', '費用'::account_type, 'P', 0),
      ('6110', '給料手当', '費用'::account_type, 'P', 0)
    """.update.apply()

    sql"""
      INSERT INTO "日次勘定科目残高" (
        "起票日", "勘定科目コード", "補助科目コード", "部門コード",
        "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
      ) VALUES
      ('2024-01-15', '4110', '', '', '', 0, 0, 10000000),
      ('2024-01-15', '5110', '', '', '', 0, 6000000, 0),
      ('2024-01-31', '6110', '', '', '', 0, 2000000, 0)
    """.update.apply()

  behavior of "GET /api/financial-statements/balance-sheet"

  it should "貸借対照表を取得できる" in withContainers { container =>
    setupRoutes(container)

    DB.localTx { implicit session =>
      clearTestData()
      setupBalanceSheetTestData()
    }

    Get("/api/financial-statements/balance-sheet?asOfDate=2024-01-31") ~> routes.routes ~> check {
      status shouldBe StatusCodes.OK
      val bs = responseAs[BalanceSheetResponse]
      bs.asOfDate shouldBe LocalDate.of(2024, 1, 31)
      bs.assets should not be empty
      bs.liabilities should not be empty
      bs.equity should not be empty
      bs.totalAssets should be > BigDecimal(0)
    }
  }

  it should "無効な日付形式で 400 を返す" in withContainers { container =>
    setupRoutes(container)

    Get("/api/financial-statements/balance-sheet?asOfDate=invalid") ~> routes.routes ~> check {
      status shouldBe StatusCodes.BadRequest
      val error = responseAs[ErrorResponse]
      error.code shouldBe "VALIDATION_ERROR"
    }
  }

  behavior of "GET /api/financial-statements/income-statement"

  it should "損益計算書を取得できる" in withContainers { container =>
    setupRoutes(container)

    DB.localTx { implicit session =>
      clearTestData()
      setupIncomeStatementTestData()
    }

    Get(
      "/api/financial-statements/income-statement?fromDate=2024-01-01&toDate=2024-01-31"
    ) ~> routes.routes ~> check {
      status shouldBe StatusCodes.OK
      val is = responseAs[IncomeStatementResponse]
      is.fromDate shouldBe LocalDate.of(2024, 1, 1)
      is.toDate shouldBe LocalDate.of(2024, 1, 31)
      is.revenues should not be empty
      is.expenses should not be empty
      is.totalRevenues should be > BigDecimal(0)
    }
  }

  it should "無効な日付形式で 400 を返す" in withContainers { container =>
    setupRoutes(container)

    Get(
      "/api/financial-statements/income-statement?fromDate=2024-01-01&toDate=invalid"
    ) ~> routes.routes ~> check {
      status shouldBe StatusCodes.BadRequest
    }
  }

  behavior of "GET /api/financial-statements/ratios"

  it should "財務指標を取得できる" in withContainers { container =>
    setupRoutes(container)

    DB.localTx { implicit session =>
      clearTestData()
      setupBalanceSheetTestData()
      setupIncomeStatementTestData()
    }

    Get(
      "/api/financial-statements/ratios?asOfDate=2024-01-31&fromDate=2024-01-01&toDate=2024-01-31"
    ) ~> routes.routes ~> check {
      status shouldBe StatusCodes.OK
      val ratios = responseAs[FinancialRatiosResponse]
      ratios.equityRatio should be >= BigDecimal(0)
      ratios.roa should be >= BigDecimal(0)
      ratios.roe should be >= BigDecimal(0)
    }
  }
