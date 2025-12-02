package com.example.accounting.api

import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport.*
import com.example.accounting.application.service.JournalService
import com.example.accounting.infrastructure.persistence.account.AccountRepository
import com.example.accounting.infrastructure.persistence.journal.JournalRepository
import com.example.accounting.infrastructure.http.JournalRoutes
import com.example.accounting.infrastructure.http.dto.*
import com.example.accounting.infrastructure.http.JsonFormats.{*, given}
import spray.json.*
import com.example.db.DatabaseSpec
import org.scalatest.BeforeAndAfterEach
import scalikejdbc.*

import java.time.LocalDate

/**
 * 仕訳 API のテスト
 */
class JournalRoutesSpec extends DatabaseSpec with ScalatestRouteTest with BeforeAndAfterEach:

  private var journalRoutes: JournalRoutes = _

  override def beforeEach(): Unit =
    super.beforeEach()

  override def afterAll(): Unit =
    super.afterAll()
    cleanUp()

  private def setupService(container: com.dimafeng.testcontainers.PostgreSQLContainer): Unit =
    setupWithMigrations(container)
    val journalRepository = JournalRepository()
    val accountRepository = AccountRepository()
    val service = JournalService(journalRepository, accountRepository)
    journalRoutes = JournalRoutes(service)

  private def clearTestData()(implicit session: DBSession): Unit =
    sql"""DELETE FROM "仕訳貸借明細"""".update.apply()
    sql"""DELETE FROM "仕訳明細"""".update.apply()
    sql"""DELETE FROM "仕訳"""".update.apply()
    sql"""DELETE FROM "日次勘定科目残高"""".update.apply()
    sql"""DELETE FROM "月次勘定科目残高"""".update.apply()
    sql"""DELETE FROM "勘定科目マスタ"""".update.apply()

  private def insertTestAccount(code: String, name: String, accountType: String = "資産")(implicit
      session: DBSession
  ): Unit =
    sql"""
      INSERT INTO "勘定科目マスタ" (
        "勘定科目コード", "勘定科目名", "勘定科目種別", "BSPL区分", "残高",
        "合計科目", "集計対象"
      ) VALUES (
        ${code}, ${name}, ${accountType}::account_type, 'B', 0, false, true
      )
    """.update.apply()

  private def insertTestJournal(
      journalNo: String,
      journalDate: LocalDate,
  )(implicit session: DBSession): Unit =
    sql"""
      INSERT INTO "仕訳" (
        "仕訳伝票番号", "起票日", "入力日", "決算仕訳フラグ", "単振フラグ",
        "仕訳伝票区分", "定期計上フラグ", "赤伝フラグ"
      ) VALUES (
        ${journalNo}, ${journalDate}, ${journalDate}, 0, 0, 0, 0, 0
      )
    """.update.apply()

  private def insertTestJournalDetail(journalNo: String, lineNo: Int, description: String)(implicit
      session: DBSession
  ): Unit =
    sql"""
      INSERT INTO "仕訳明細" (
        "仕訳伝票番号", "仕訳行番号", "行摘要"
      ) VALUES (
        ${journalNo}, ${lineNo}, ${description}
      )
    """.update.apply()

  private def insertTestDebitCreditDetail(
      journalNo: String,
      lineNo: Int,
      dcType: String,
      accountCode: String,
      amount: BigDecimal,
  )(implicit session: DBSession): Unit =
    sql"""
      INSERT INTO "仕訳貸借明細" (
        "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分", "通貨コード", "為替レート",
        "勘定科目コード", "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
      ) VALUES (
        ${journalNo}, ${lineNo}, ${dcType}, 'JPY', 1.0,
        ${accountCode}, ${amount}, ${amount}, 0
      )
    """.update.apply()

  behavior of "GET /api/journals?from=...&to=..."

  it should "期間指定で仕訳を取得できる" in withContainers { container =>
    setupService(container)

    DB.localTx { implicit session =>
      clearTestData()
      insertTestAccount("1110", "現金")
      insertTestAccount("4110", "売上高", "収益")
      insertTestJournal("J001", LocalDate.of(2024, 1, 15))
      insertTestJournalDetail("J001", 1, "売上計上")
      insertTestDebitCreditDetail("J001", 1, "D", "1110", BigDecimal(10000))
      insertTestDebitCreditDetail("J001", 1, "C", "4110", BigDecimal(10000))
    }

    Get("/api/journals?from=2024-01-01&to=2024-01-31") ~> journalRoutes.routes ~> check {
      status shouldBe StatusCodes.OK
      val jsonStr = responseAs[String]
      println(s"DEBUG JSON: $jsonStr")
      val json = jsonStr.parseJson
      json shouldBe a[JsArray]
      val arr = json.asInstanceOf[JsArray]
      arr.elements should have length 1
      val firstJournal = arr.elements.head.asJsObject
      firstJournal.fields("journalNo").convertTo[String] shouldBe "J001"
      firstJournal.fields("isBalanced").convertTo[Boolean] shouldBe true
    }
  }

  it should "期間外の仕訳は取得されない" in withContainers { container =>
    setupService(container)

    DB.localTx { implicit session =>
      clearTestData()
      insertTestAccount("1110", "現金")
      insertTestAccount("4110", "売上高", "収益")
      insertTestJournal("J001", LocalDate.of(2024, 1, 15))
      insertTestJournalDetail("J001", 1, "売上計上")
      insertTestDebitCreditDetail("J001", 1, "D", "1110", BigDecimal(10000))
      insertTestDebitCreditDetail("J001", 1, "C", "4110", BigDecimal(10000))
    }

    Get("/api/journals?from=2024-02-01&to=2024-02-28") ~> journalRoutes.routes ~> check {
      status shouldBe StatusCodes.OK
      val jsonStr = responseAs[String]
      val json = jsonStr.parseJson.asInstanceOf[JsArray]
      json.elements shouldBe empty
    }
  }

  it should "不正な日付形式で 400 を返す" in withContainers { container =>
    setupService(container)

    Get("/api/journals?from=invalid&to=2024-01-31") ~> journalRoutes.routes ~> check {
      status shouldBe StatusCodes.BadRequest
      val jsonStr = responseAs[String]
      val error = jsonStr.parseJson.convertTo[ErrorResponse]
      error.code shouldBe "VALIDATION_ERROR"
    }
  }

  behavior of "GET /api/journals/:journalNo"

  it should "仕訳番号で取得できる" in withContainers { container =>
    setupService(container)

    DB.localTx { implicit session =>
      clearTestData()
      insertTestAccount("1110", "現金")
      insertTestAccount("4110", "売上高", "収益")
      insertTestJournal("J001", LocalDate.of(2024, 1, 15))
      insertTestJournalDetail("J001", 1, "売上計上")
      insertTestDebitCreditDetail("J001", 1, "D", "1110", BigDecimal(10000))
      insertTestDebitCreditDetail("J001", 1, "C", "4110", BigDecimal(10000))
    }

    Get("/api/journals/J001") ~> journalRoutes.routes ~> check {
      status shouldBe StatusCodes.OK
      val jsonStr = responseAs[String]
      val journal = jsonStr.parseJson.asJsObject
      journal.fields("journalNo").convertTo[String] shouldBe "J001"
      journal.fields("totalDebit").convertTo[BigDecimal] shouldBe BigDecimal(10000)
      journal.fields("totalCredit").convertTo[BigDecimal] shouldBe BigDecimal(10000)
      journal.fields("isBalanced").convertTo[Boolean] shouldBe true
    }
  }

  it should "存在しない仕訳番号で 404 を返す" in withContainers { container =>
    setupService(container)

    DB.localTx { implicit session =>
      clearTestData()
    }

    Get("/api/journals/NOTEXIST") ~> journalRoutes.routes ~> check {
      status shouldBe StatusCodes.NotFound
      val jsonStr = responseAs[String]
      val error = jsonStr.parseJson.convertTo[ErrorResponse]
      error.code shouldBe "NOT_FOUND"
    }
  }

  behavior of "POST /api/journals"

  it should "仕訳を作成できる" in withContainers { container =>
    setupService(container)

    DB.localTx { implicit session =>
      clearTestData()
      insertTestAccount("1110", "現金")
      insertTestAccount("4110", "売上高", "収益")
    }

    val request = JournalRequest(
      journalNo = "J002",
      journalDate = LocalDate.of(2024, 1, 20),
      inputDate = LocalDate.of(2024, 1, 20),
      settlementFlag = 0,
      singleEntryFlag = 0,
      journalType = 0,
      recurringFlag = 0,
      employeeCode = None,
      departmentCode = None,
      redSlipFlag = 0,
      redBlackVoucherNo = None,
      details = List(
        JournalDetailRequest(lineNumber = 1, lineDescription = "現金売上")
      ),
      debitCreditDetails = List(
        JournalDebitCreditDetailRequest(
          lineNumber = 1,
          debitCreditType = "D",
          currencyCode = "JPY",
          exchangeRate = BigDecimal(1),
          departmentCode = None,
          projectCode = None,
          accountCode = "1110",
          subAccountCode = None,
          amount = BigDecimal(50000),
          baseAmount = BigDecimal(50000),
          taxType = None,
          taxRate = None,
          taxCalculationType = None,
          dueDate = None,
          cashFlowFlag = 0,
          segmentCode = None,
          counterAccountCode = None,
          counterSubAccountCode = None,
          memoCode = None,
          memoContent = None,
        ),
        JournalDebitCreditDetailRequest(
          lineNumber = 1,
          debitCreditType = "C",
          currencyCode = "JPY",
          exchangeRate = BigDecimal(1),
          departmentCode = None,
          projectCode = None,
          accountCode = "4110",
          subAccountCode = None,
          amount = BigDecimal(50000),
          baseAmount = BigDecimal(50000),
          taxType = None,
          taxRate = None,
          taxCalculationType = None,
          dueDate = None,
          cashFlowFlag = 0,
          segmentCode = None,
          counterAccountCode = None,
          counterSubAccountCode = None,
          memoCode = None,
          memoContent = None,
        ),
      ),
    )

    Post("/api/journals", request) ~> journalRoutes.routes ~> check {
      status shouldBe StatusCodes.Created
      val jsonStr = responseAs[String]
      val journal = jsonStr.parseJson.asJsObject
      journal.fields("journalNo").convertTo[String] shouldBe "J002"
      journal.fields("totalDebit").convertTo[BigDecimal] shouldBe BigDecimal(50000)
      journal.fields("totalCredit").convertTo[BigDecimal] shouldBe BigDecimal(50000)
      journal.fields("isBalanced").convertTo[Boolean] shouldBe true
    }
  }

  it should "貸借不一致の仕訳で 400 を返す" in withContainers { container =>
    setupService(container)

    DB.localTx { implicit session =>
      clearTestData()
      insertTestAccount("1110", "現金")
      insertTestAccount("4110", "売上高", "収益")
    }

    val request = JournalRequest(
      journalNo = "J003",
      journalDate = LocalDate.of(2024, 1, 20),
      inputDate = LocalDate.of(2024, 1, 20),
      settlementFlag = 0,
      singleEntryFlag = 0,
      journalType = 0,
      recurringFlag = 0,
      employeeCode = None,
      departmentCode = None,
      redSlipFlag = 0,
      redBlackVoucherNo = None,
      details = List(
        JournalDetailRequest(lineNumber = 1, lineDescription = "不一致テスト")
      ),
      debitCreditDetails = List(
        JournalDebitCreditDetailRequest(
          lineNumber = 1,
          debitCreditType = "D",
          currencyCode = "JPY",
          exchangeRate = BigDecimal(1),
          departmentCode = None,
          projectCode = None,
          accountCode = "1110",
          subAccountCode = None,
          amount = BigDecimal(50000),
          baseAmount = BigDecimal(50000),
          taxType = None,
          taxRate = None,
          taxCalculationType = None,
          dueDate = None,
          cashFlowFlag = 0,
          segmentCode = None,
          counterAccountCode = None,
          counterSubAccountCode = None,
          memoCode = None,
          memoContent = None,
        ),
        JournalDebitCreditDetailRequest(
          lineNumber = 1,
          debitCreditType = "C",
          currencyCode = "JPY",
          exchangeRate = BigDecimal(1),
          departmentCode = None,
          projectCode = None,
          accountCode = "4110",
          subAccountCode = None,
          amount = BigDecimal(30000),
          baseAmount = BigDecimal(30000),
          taxType = None,
          taxRate = None,
          taxCalculationType = None,
          dueDate = None,
          cashFlowFlag = 0,
          segmentCode = None,
          counterAccountCode = None,
          counterSubAccountCode = None,
          memoCode = None,
          memoContent = None,
        ),
      ),
    )

    Post("/api/journals", request) ~> journalRoutes.routes ~> check {
      status shouldBe StatusCodes.BadRequest
      val jsonStr = responseAs[String]
      val error = jsonStr.parseJson.convertTo[ErrorResponse]
      error.code shouldBe "INVALID_JOURNAL"
    }
  }

  behavior of "DELETE /api/journals/:journalNo"

  it should "仕訳を削除できる" in withContainers { container =>
    setupService(container)

    DB.localTx { implicit session =>
      clearTestData()
      insertTestAccount("1110", "現金")
      insertTestAccount("4110", "売上高", "収益")
      insertTestJournal("J001", LocalDate.of(2024, 1, 15))
      insertTestJournalDetail("J001", 1, "売上計上")
      insertTestDebitCreditDetail("J001", 1, "D", "1110", BigDecimal(10000))
      insertTestDebitCreditDetail("J001", 1, "C", "4110", BigDecimal(10000))
    }

    Delete("/api/journals/J001") ~> journalRoutes.routes ~> check {
      status shouldBe StatusCodes.NoContent
    }

    // 削除後に取得を試みる
    Get("/api/journals/J001") ~> journalRoutes.routes ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  it should "存在しない仕訳の削除で 404 を返す" in withContainers { container =>
    setupService(container)

    DB.localTx { implicit session =>
      clearTestData()
    }

    Delete("/api/journals/NOTEXIST") ~> journalRoutes.routes ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  behavior of "GET /api/journals/:journalNo/validate"

  it should "貸借一致の仕訳を検証できる" in withContainers { container =>
    setupService(container)

    DB.localTx { implicit session =>
      clearTestData()
      insertTestAccount("1110", "現金")
      insertTestAccount("4110", "売上高", "収益")
      insertTestJournal("J001", LocalDate.of(2024, 1, 15))
      insertTestJournalDetail("J001", 1, "売上計上")
      insertTestDebitCreditDetail("J001", 1, "D", "1110", BigDecimal(10000))
      insertTestDebitCreditDetail("J001", 1, "C", "4110", BigDecimal(10000))
    }

    Get("/api/journals/J001/validate") ~> journalRoutes.routes ~> check {
      status shouldBe StatusCodes.OK
      val jsonStr = responseAs[String]
      val validation = jsonStr.parseJson.asJsObject
      validation.fields("journalNo").convertTo[String] shouldBe "J001"
      validation.fields("isBalanced").convertTo[Boolean] shouldBe true
      validation.fields("totalDebit").convertTo[BigDecimal] shouldBe BigDecimal(10000)
      validation.fields("totalCredit").convertTo[BigDecimal] shouldBe BigDecimal(10000)
    }
  }

  it should "存在しない仕訳の検証で 404 を返す" in withContainers { container =>
    setupService(container)

    DB.localTx { implicit session =>
      clearTestData()
    }

    Get("/api/journals/NOTEXIST/validate") ~> journalRoutes.routes ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }
