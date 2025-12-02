package com.example.accounting.api

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport.*
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.example.db.DatabaseSpec
import org.scalatest.BeforeAndAfterEach
import scalikejdbc.*
import spray.json.*

/**
 * 仕訳エントリ API（イベントソーシング）のスモークテスト
 */
class JournalEntryRoutesSpec extends DatabaseSpec with ScalatestRouteTest with BeforeAndAfterEach:

  import com.example.accounting.application.service.JournalEntryService
  import com.example.accounting.infrastructure.out.persistence.eventsourcing.{EventStoreRepositoryImpl, JournalReadModelRepositoryImpl}
  import com.example.accounting.infrastructure.in.http.JournalEntryRoutes
  import com.example.accounting.infrastructure.in.http.dto.*
  import com.example.accounting.infrastructure.in.http.JsonFormats.{*, given}

  private var journalEntryRoutes: JournalEntryRoutes = _

  override def beforeEach(): Unit =
    super.beforeEach()

  override def afterAll(): Unit =
    super.afterAll()
    cleanUp()

  private def setupService(container: com.dimafeng.testcontainers.PostgreSQLContainer): Unit =
    setupWithMigrations(container)
    val eventStoreRepository = EventStoreRepositoryImpl()
    val journalReadModelRepository = JournalReadModelRepositoryImpl()
    val service = JournalEntryService(eventStoreRepository, journalReadModelRepository)
    journalEntryRoutes = JournalEntryRoutes(service)

  private def clearTestData()(implicit session: DBSession): Unit =
    sql"""DELETE FROM "仕訳リードモデル"""".update.apply()
    sql"""DELETE FROM "集約スナップショット"""".update.apply()
    sql"""DELETE FROM "イベントストア"""".update.apply()

  private def createJournalEntryRequest: String =
    """
    {
      "journalDate": "2024-01-15",
      "description": "テスト仕訳",
      "details": [
        {
          "lineNumber": 1,
          "accountCode": "1000",
          "debitCredit": "D",
          "amount": 10000,
          "description": "借方明細"
        },
        {
          "lineNumber": 2,
          "accountCode": "4000",
          "debitCredit": "C",
          "amount": 10000,
          "description": "貸方明細"
        }
      ],
      "createdBy": "user1"
    }
    """

  behavior of "POST /api/journal-entries"

  it should "仕訳エントリを作成できる" in withContainers { container =>
    setupService(container)

    DB.localTx { implicit session =>
      clearTestData()
    }

    Post("/api/journal-entries", HttpEntity(ContentTypes.`application/json`, createJournalEntryRequest)) ~>
      journalEntryRoutes.routes ~> check {
        status shouldBe StatusCodes.Created
        val response = responseAs[JournalEntryResponse]
        response.description shouldBe Some("テスト仕訳")
        response.status shouldBe "Draft"
        response.totalDebit shouldBe BigDecimal(10000)
        response.totalCredit shouldBe BigDecimal(10000)
        response.isBalanced shouldBe true
      }
  }

  it should "貸借不均衡の仕訳は 400 を返す" in withContainers { container =>
    setupService(container)

    val unbalancedRequest =
      """
      {
        "journalDate": "2024-01-15",
        "description": "不均衡仕訳",
        "details": [
          {
            "lineNumber": 1,
            "accountCode": "1000",
            "debitCredit": "D",
            "amount": 10000,
            "description": "借方明細"
          },
          {
            "lineNumber": 2,
            "accountCode": "4000",
            "debitCredit": "C",
            "amount": 5000,
            "description": "貸方明細"
          }
        ],
        "createdBy": "user1"
      }
      """

    Post("/api/journal-entries", HttpEntity(ContentTypes.`application/json`, unbalancedRequest)) ~>
      journalEntryRoutes.routes ~> check {
        status shouldBe StatusCodes.BadRequest
        val error = responseAs[ErrorResponse]
        error.code shouldBe "INVALID_JOURNAL"
      }
  }

  behavior of "GET /api/journal-entries/:id"

  it should "仕訳エントリを取得できる" in withContainers { container =>
    setupService(container)

    DB.localTx { implicit session =>
      clearTestData()
    }

    // 作成
    var aggregateId: String = ""
    Post("/api/journal-entries", HttpEntity(ContentTypes.`application/json`, createJournalEntryRequest)) ~>
      journalEntryRoutes.routes ~> check {
        status shouldBe StatusCodes.Created
        aggregateId = responseAs[JournalEntryResponse].id
      }

    // 取得
    Get(s"/api/journal-entries/$aggregateId") ~> journalEntryRoutes.routes ~> check {
      status shouldBe StatusCodes.OK
      val response = responseAs[JournalEntryResponse]
      response.id shouldBe aggregateId
      response.status shouldBe "Draft"
    }
  }

  it should "存在しない仕訳エントリは 404 を返す" in withContainers { container =>
    setupService(container)

    Get("/api/journal-entries/nonexistent-id") ~> journalEntryRoutes.routes ~> check {
      status shouldBe StatusCodes.NotFound
      val error = responseAs[ErrorResponse]
      error.code shouldBe "NOT_FOUND"
    }
  }

  behavior of "POST /api/journal-entries/:id/approve"

  it should "仕訳エントリを承認できる" in withContainers { container =>
    setupService(container)

    DB.localTx { implicit session =>
      clearTestData()
    }

    // 作成
    var aggregateId: String = ""
    Post("/api/journal-entries", HttpEntity(ContentTypes.`application/json`, createJournalEntryRequest)) ~>
      journalEntryRoutes.routes ~> check {
        status shouldBe StatusCodes.Created
        aggregateId = responseAs[JournalEntryResponse].id
      }

    // 承認
    val approveRequest = """{"approvedBy": "approver1"}"""
    Post(s"/api/journal-entries/$aggregateId/approve", HttpEntity(ContentTypes.`application/json`, approveRequest)) ~>
      journalEntryRoutes.routes ~> check {
        status shouldBe StatusCodes.OK
        val response = responseAs[JournalEntryResponse]
        response.status shouldBe "Approved"
        response.approvedBy shouldBe Some("approver1")
      }
  }

  it should "承認済み仕訳は再承認できない" in withContainers { container =>
    setupService(container)

    DB.localTx { implicit session =>
      clearTestData()
    }

    // 作成
    var aggregateId: String = ""
    Post("/api/journal-entries", HttpEntity(ContentTypes.`application/json`, createJournalEntryRequest)) ~>
      journalEntryRoutes.routes ~> check {
        status shouldBe StatusCodes.Created
        aggregateId = responseAs[JournalEntryResponse].id
      }

    // 承認
    val approveRequest = """{"approvedBy": "approver1"}"""
    Post(s"/api/journal-entries/$aggregateId/approve", HttpEntity(ContentTypes.`application/json`, approveRequest)) ~>
      journalEntryRoutes.routes ~> check {
        status shouldBe StatusCodes.OK
      }

    // 再承認（失敗）
    Post(s"/api/journal-entries/$aggregateId/approve", HttpEntity(ContentTypes.`application/json`, approveRequest)) ~>
      journalEntryRoutes.routes ~> check {
        status shouldBe StatusCodes.BadRequest
        val error = responseAs[ErrorResponse]
        error.code shouldBe "VALIDATION_ERROR"
      }
  }

  behavior of "POST /api/journal-entries/:id/reject"

  it should "仕訳エントリを却下できる" in withContainers { container =>
    setupService(container)

    DB.localTx { implicit session =>
      clearTestData()
    }

    // 作成
    var aggregateId: String = ""
    Post("/api/journal-entries", HttpEntity(ContentTypes.`application/json`, createJournalEntryRequest)) ~>
      journalEntryRoutes.routes ~> check {
        status shouldBe StatusCodes.Created
        aggregateId = responseAs[JournalEntryResponse].id
      }

    // 却下
    val rejectRequest = """{"rejectedBy": "reviewer1", "reason": "金額が不正"}"""
    Post(s"/api/journal-entries/$aggregateId/reject", HttpEntity(ContentTypes.`application/json`, rejectRequest)) ~>
      journalEntryRoutes.routes ~> check {
        status shouldBe StatusCodes.OK
        val response = responseAs[JournalEntryResponse]
        response.status shouldBe "Rejected"
      }
  }

  behavior of "DELETE /api/journal-entries/:id"

  it should "仕訳エントリを削除できる" in withContainers { container =>
    setupService(container)

    DB.localTx { implicit session =>
      clearTestData()
    }

    // 作成
    var aggregateId: String = ""
    Post("/api/journal-entries", HttpEntity(ContentTypes.`application/json`, createJournalEntryRequest)) ~>
      journalEntryRoutes.routes ~> check {
        status shouldBe StatusCodes.Created
        aggregateId = responseAs[JournalEntryResponse].id
      }

    // 削除
    val deleteRequest = """{"deletedBy": "admin1", "reason": "重複仕訳"}"""
    Delete(s"/api/journal-entries/$aggregateId", HttpEntity(ContentTypes.`application/json`, deleteRequest)) ~>
      journalEntryRoutes.routes ~> check {
        status shouldBe StatusCodes.OK
        val response = responseAs[JournalEntryResponse]
        response.status shouldBe "Deleted"
      }
  }

  behavior of "GET /api/journal-entries/:id/history"

  it should "仕訳エントリのイベント履歴を取得できる" in withContainers { container =>
    setupService(container)

    DB.localTx { implicit session =>
      clearTestData()
    }

    // 作成
    var aggregateId: String = ""
    Post("/api/journal-entries", HttpEntity(ContentTypes.`application/json`, createJournalEntryRequest)) ~>
      journalEntryRoutes.routes ~> check {
        status shouldBe StatusCodes.Created
        aggregateId = responseAs[JournalEntryResponse].id
      }

    // 承認
    val approveRequest = """{"approvedBy": "approver1"}"""
    Post(s"/api/journal-entries/$aggregateId/approve", HttpEntity(ContentTypes.`application/json`, approveRequest)) ~>
      journalEntryRoutes.routes ~> check {
        status shouldBe StatusCodes.OK
      }

    // 履歴取得
    Get(s"/api/journal-entries/$aggregateId/history") ~> journalEntryRoutes.routes ~> check {
      status shouldBe StatusCodes.OK
      val events = responseAs[List[JournalEntryEventResponse]]
      events should have size 2
      events(0).eventType shouldBe "JournalEntryCreatedEvent"
      events(1).eventType shouldBe "JournalEntryApprovedEvent"
    }
  }

  it should "存在しない仕訳の履歴は 404 を返す" in withContainers { container =>
    setupService(container)

    Get("/api/journal-entries/nonexistent-id/history") ~> journalEntryRoutes.routes ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }
