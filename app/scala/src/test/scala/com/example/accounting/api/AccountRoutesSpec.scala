package com.example.accounting.api

import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport.*
import com.example.accounting.application.AccountService
import com.example.accounting.domain.AccountType
import com.example.accounting.infrastructure.persistence.AccountRepository
import com.example.accounting.infrastructure.http.AccountRoutes
import com.example.accounting.infrastructure.http.dto.*
import com.example.accounting.infrastructure.http.JsonFormats.{*, given}
import com.example.db.DatabaseSpec
import org.scalatest.BeforeAndAfterEach
import scalikejdbc.*

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

/**
 * 勘定科目 API のテスト
 */
class AccountRoutesSpec extends DatabaseSpec with ScalatestRouteTest with BeforeAndAfterEach:

  private var accountRoutes: AccountRoutes = _

  override def beforeEach(): Unit =
    super.beforeEach()

  override def afterAll(): Unit =
    super.afterAll()
    cleanUp()

  private def setupService(container: com.dimafeng.testcontainers.PostgreSQLContainer): Unit =
    setupWithMigrations(container)
    val repository = AccountRepository()
    val service = AccountService(repository)
    accountRoutes = AccountRoutes(service)

  private def clearTestData()(implicit session: DBSession): Unit =
    sql"""DELETE FROM "日次勘定科目残高"""".update.apply()
    sql"""DELETE FROM "月次勘定科目残高"""".update.apply()
    sql"""DELETE FROM "勘定科目マスタ"""".update.apply()

  private def insertTestAccount(
      code: String,
      name: String,
      accountType: String = "資産",
  )(implicit session: DBSession): Unit =
    sql"""
      INSERT INTO "勘定科目マスタ" (
        "勘定科目コード", "勘定科目名", "勘定科目種別", "BSPL区分", "残高",
        "合計科目", "集計対象"
      ) VALUES (
        ${code}, ${name}, ${accountType}::account_type, 'B', 0, false, true
      )
    """.update.apply()

  behavior of "GET /api/accounts"

  it should "すべての勘定科目を取得できる" in withContainers { container =>
    setupService(container)

    DB.localTx { implicit session =>
      clearTestData()
      insertTestAccount("1110", "現金")
      insertTestAccount("1120", "普通預金")
    }

    Get("/api/accounts") ~> accountRoutes.routes ~> check {
      status shouldBe StatusCodes.OK
      val accounts = responseAs[List[AccountResponse]]
      accounts should have length 2
      accounts.map(_.accountCode) should contain allOf ("1110", "1120")
    }
  }

  behavior of "GET /api/accounts/:code"

  it should "勘定科目コードで取得できる" in withContainers { container =>
    setupService(container)

    DB.localTx { implicit session =>
      clearTestData()
      insertTestAccount("1110", "現金")
    }

    Get("/api/accounts/1110") ~> accountRoutes.routes ~> check {
      status shouldBe StatusCodes.OK
      val account = responseAs[AccountResponse]
      account.accountCode shouldBe "1110"
      account.accountName shouldBe "現金"
    }
  }

  it should "存在しない勘定科目コードで 404 を返す" in withContainers { container =>
    setupService(container)

    DB.localTx { implicit session =>
      clearTestData()
    }

    Get("/api/accounts/9999") ~> accountRoutes.routes ~> check {
      status shouldBe StatusCodes.NotFound
      val error = responseAs[ErrorResponse]
      error.code shouldBe "NOT_FOUND"
    }
  }

  behavior of "POST /api/accounts"

  it should "勘定科目を作成できる" in withContainers { container =>
    setupService(container)

    DB.localTx { implicit session =>
      clearTestData()
    }

    val request = AccountRequest(
      accountCode = "1130",
      accountName = "定期預金",
      accountType = "資産",
      balance = BigDecimal(0),
      bsplDistinction = Some("B"),
      transactionElement = None,
      expenseDistinction = None,
      isSummaryAccount = false,
      displayOrder = Some(3),
      isAggregationTarget = true,
      accountKana = Some("テイキヨキン"),
      taxCode = None,
    )

    Post("/api/accounts", request) ~> accountRoutes.routes ~> check {
      status shouldBe StatusCodes.Created
      val account = responseAs[AccountResponse]
      account.accountCode shouldBe "1130"
      account.accountName shouldBe "定期預金"
    }
  }

  it should "重複する勘定科目コードで 409 を返す" in withContainers { container =>
    setupService(container)

    DB.localTx { implicit session =>
      clearTestData()
      insertTestAccount("1110", "現金")
    }

    val request = AccountRequest(
      accountCode = "1110",
      accountName = "重複テスト",
      accountType = "資産",
      balance = BigDecimal(0),
      bsplDistinction = Some("B"),
      transactionElement = None,
      expenseDistinction = None,
      isSummaryAccount = false,
      displayOrder = None,
      isAggregationTarget = true,
      accountKana = None,
      taxCode = None,
    )

    Post("/api/accounts", request) ~> accountRoutes.routes ~> check {
      status shouldBe StatusCodes.Conflict
      val error = responseAs[ErrorResponse]
      error.code shouldBe "DUPLICATE_ERROR"
    }
  }

  behavior of "PUT /api/accounts/:code"

  it should "勘定科目を更新できる" in withContainers { container =>
    setupService(container)

    DB.localTx { implicit session =>
      clearTestData()
      insertTestAccount("1110", "現金")
    }

    val request = AccountRequest(
      accountCode = "1110",
      accountName = "現金（更新後）",
      accountType = "資産",
      balance = BigDecimal(100000),
      bsplDistinction = Some("B"),
      transactionElement = None,
      expenseDistinction = None,
      isSummaryAccount = false,
      displayOrder = Some(1),
      isAggregationTarget = true,
      accountKana = Some("ゲンキン"),
      taxCode = None,
    )

    Put("/api/accounts/1110", request) ~> accountRoutes.routes ~> check {
      status shouldBe StatusCodes.OK
      val account = responseAs[AccountResponse]
      account.accountName shouldBe "現金（更新後）"
      account.balance shouldBe BigDecimal(100000)
    }
  }

  it should "存在しない勘定科目の更新で 404 を返す" in withContainers { container =>
    setupService(container)

    DB.localTx { implicit session =>
      clearTestData()
    }

    val request = AccountRequest(
      accountCode = "9999",
      accountName = "存在しない",
      accountType = "資産",
      balance = BigDecimal(0),
      bsplDistinction = Some("B"),
      transactionElement = None,
      expenseDistinction = None,
      isSummaryAccount = false,
      displayOrder = None,
      isAggregationTarget = true,
      accountKana = None,
      taxCode = None,
    )

    Put("/api/accounts/9999", request) ~> accountRoutes.routes ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  behavior of "DELETE /api/accounts/:code"

  it should "勘定科目を削除できる" in withContainers { container =>
    setupService(container)

    DB.localTx { implicit session =>
      clearTestData()
      insertTestAccount("1110", "現金")
    }

    Delete("/api/accounts/1110") ~> accountRoutes.routes ~> check {
      status shouldBe StatusCodes.NoContent
    }

    // 削除後に取得を試みる
    Get("/api/accounts/1110") ~> accountRoutes.routes ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  it should "存在しない勘定科目の削除で 404 を返す" in withContainers { container =>
    setupService(container)

    DB.localTx { implicit session =>
      clearTestData()
    }

    Delete("/api/accounts/9999") ~> accountRoutes.routes ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  behavior of "GET /api/accounts/type/:type"

  it should "勘定科目種別で検索できる" in withContainers { container =>
    setupService(container)

    DB.localTx { implicit session =>
      clearTestData()
      insertTestAccount("1110", "現金", "資産")
      insertTestAccount("1120", "普通預金", "資産")
      insertTestAccount("2110", "買掛金", "負債")
    }

    val encodedType = URLEncoder.encode("資産", StandardCharsets.UTF_8)
    Get(s"/api/accounts/type/$encodedType") ~> accountRoutes.routes ~> check {
      status shouldBe StatusCodes.OK
      val accounts = responseAs[List[AccountResponse]]
      accounts should have length 2
      accounts.foreach(_.accountType shouldBe "資産")
    }
  }

  it should "無効な勘定科目種別で 400 を返す" in withContainers { container =>
    setupService(container)

    val encodedType = URLEncoder.encode("無効", StandardCharsets.UTF_8)
    Get(s"/api/accounts/type/$encodedType") ~> accountRoutes.routes ~> check {
      status shouldBe StatusCodes.BadRequest
      val error = responseAs[ErrorResponse]
      error.code shouldBe "VALIDATION_ERROR"
    }
  }
