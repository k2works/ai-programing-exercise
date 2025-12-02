package com.example.accounting.api

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport.*
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.example.db.DatabaseSpec
import org.scalatest.BeforeAndAfterEach
import scalikejdbc.*
import spray.json.*

import java.time.{OffsetDateTime, ZoneOffset}

/**
 * 監査ログ API のスモークテスト
 */
class AuditLogRoutesSpec extends DatabaseSpec with ScalatestRouteTest with BeforeAndAfterEach:

  import com.example.accounting.application.service.AuditLogService
  import com.example.accounting.infrastructure.out.persistence.audit.AuditLogRepository
  import com.example.accounting.infrastructure.in.http.AuditLogRoutes
  import com.example.accounting.infrastructure.in.http.dto.*
  import com.example.accounting.infrastructure.in.http.JsonFormats.{*, given}

  private var auditLogRoutes: AuditLogRoutes = _

  override def beforeEach(): Unit =
    super.beforeEach()

  override def afterAll(): Unit =
    super.afterAll()
    cleanUp()

  private def setupService(container: com.dimafeng.testcontainers.PostgreSQLContainer): Unit =
    setupWithMigrations(container)
    val repository = AuditLogRepository()
    val service = AuditLogService(repository)
    auditLogRoutes = AuditLogRoutes(service)

  private def clearTestData()(implicit session: DBSession): Unit =
    sql"""DELETE FROM "監査ログ"""".update.apply()

  private def insertTestAuditLog(
      entityType: String,
      entityId: String,
      action: String,
      userId: String,
      userName: String,
      timestamp: OffsetDateTime,
  )(implicit session: DBSession): Long =
    sql"""
      INSERT INTO "監査ログ" (
        "エンティティ種別", "エンティティID", "アクション",
        "ユーザーID", "ユーザー名", "タイムスタンプ"
      ) VALUES (
        ${entityType}, ${entityId}, ${action},
        ${userId}, ${userName}, ${timestamp}
      )
    """.updateAndReturnGeneratedKey.apply()

  behavior of "GET /api/audit-logs"

  it should "期間指定で監査ログを取得できる" in withContainers { container =>
    setupService(container)

    val now = OffsetDateTime.now(ZoneOffset.UTC)
    DB.localTx { implicit session =>
      clearTestData()
      insertTestAuditLog("Account", "1110", "CREATE", "user1", "テストユーザー1", now.minusDays(1))
      insertTestAuditLog("Journal", "J001", "UPDATE", "user2", "テストユーザー2", now.minusHours(1))
    }

    val startDate = now.minusDays(30).toInstant.toString
    val endDate = now.toInstant.toString

    Get(s"/api/audit-logs?startDate=$startDate&endDate=$endDate&limit=100") ~> auditLogRoutes.routes ~> check {
      status shouldBe StatusCodes.OK
      val jsonStr = responseAs[String]
      val json = jsonStr.parseJson
      json shouldBe a[JsArray]
      val arr = json.asInstanceOf[JsArray]
      arr.elements should have length 2
    }
  }

  it should "デフォルトパラメータで監査ログを取得できる" in withContainers { container =>
    setupService(container)

    val now = OffsetDateTime.now(ZoneOffset.UTC)
    DB.localTx { implicit session =>
      clearTestData()
      insertTestAuditLog("Account", "1110", "CREATE", "user1", "テストユーザー1", now.minusDays(1))
    }

    Get("/api/audit-logs") ~> auditLogRoutes.routes ~> check {
      status shouldBe StatusCodes.OK
      val jsonStr = responseAs[String]
      val json = jsonStr.parseJson
      json shouldBe a[JsArray]
    }
  }

  it should "無効な日付形式で 400 を返す" in withContainers { container =>
    setupService(container)

    Get("/api/audit-logs?startDate=invalid-date") ~> auditLogRoutes.routes ~> check {
      status shouldBe StatusCodes.BadRequest
      val error = responseAs[ErrorResponse]
      error.code shouldBe "VALIDATION_ERROR"
    }
  }

  behavior of "GET /api/audit-logs/entity/:entityType/:entityId"

  it should "エンティティで監査ログを取得できる" in withContainers { container =>
    setupService(container)

    val now = OffsetDateTime.now(ZoneOffset.UTC)
    DB.localTx { implicit session =>
      clearTestData()
      insertTestAuditLog("Account", "1110", "CREATE", "user1", "テストユーザー1", now.minusDays(2))
      insertTestAuditLog("Account", "1110", "UPDATE", "user1", "テストユーザー1", now.minusDays(1))
      insertTestAuditLog("Account", "1120", "CREATE", "user2", "テストユーザー2", now.minusHours(1))
    }

    Get("/api/audit-logs/entity/Account/1110") ~> auditLogRoutes.routes ~> check {
      status shouldBe StatusCodes.OK
      val jsonStr = responseAs[String]
      val json = jsonStr.parseJson.asInstanceOf[JsArray]
      json.elements should have length 2
      json.elements.foreach { elem =>
        val obj = elem.asJsObject
        obj.fields("entityType").convertTo[String] shouldBe "Account"
        obj.fields("entityId").convertTo[String] shouldBe "1110"
      }
    }
  }

  it should "存在しないエンティティで空のリストを返す" in withContainers { container =>
    setupService(container)

    DB.localTx { implicit session =>
      clearTestData()
    }

    Get("/api/audit-logs/entity/Account/9999") ~> auditLogRoutes.routes ~> check {
      status shouldBe StatusCodes.OK
      val jsonStr = responseAs[String]
      val json = jsonStr.parseJson.asInstanceOf[JsArray]
      json.elements shouldBe empty
    }
  }

  behavior of "GET /api/audit-logs/user/:userId"

  it should "ユーザーIDで監査ログを取得できる" in withContainers { container =>
    setupService(container)

    val now = OffsetDateTime.now(ZoneOffset.UTC)
    DB.localTx { implicit session =>
      clearTestData()
      insertTestAuditLog("Account", "1110", "CREATE", "user1", "テストユーザー1", now.minusDays(1))
      insertTestAuditLog("Journal", "J001", "CREATE", "user1", "テストユーザー1", now.minusHours(1))
      insertTestAuditLog("Account", "1120", "CREATE", "user2", "テストユーザー2", now.minusHours(2))
    }

    val startDate = now.minusDays(30).toInstant.toString
    val endDate = now.toInstant.toString

    Get(s"/api/audit-logs/user/user1?startDate=$startDate&endDate=$endDate") ~> auditLogRoutes.routes ~> check {
      status shouldBe StatusCodes.OK
      val jsonStr = responseAs[String]
      val json = jsonStr.parseJson.asInstanceOf[JsArray]
      json.elements should have length 2
      json.elements.foreach { elem =>
        elem.asJsObject.fields("userId").convertTo[String] shouldBe "user1"
      }
    }
  }

  it should "存在しないユーザーで空のリストを返す" in withContainers { container =>
    setupService(container)

    DB.localTx { implicit session =>
      clearTestData()
    }

    Get("/api/audit-logs/user/nonexistent") ~> auditLogRoutes.routes ~> check {
      status shouldBe StatusCodes.OK
      val jsonStr = responseAs[String]
      val json = jsonStr.parseJson.asInstanceOf[JsArray]
      json.elements shouldBe empty
    }
  }

  behavior of "GET /api/audit-logs/action/:action"

  it should "アクションで監査ログを取得できる" in withContainers { container =>
    setupService(container)

    val now = OffsetDateTime.now(ZoneOffset.UTC)
    DB.localTx { implicit session =>
      clearTestData()
      insertTestAuditLog("Account", "1110", "CREATE", "user1", "テストユーザー1", now.minusDays(1))
      insertTestAuditLog("Account", "1110", "UPDATE", "user1", "テストユーザー1", now.minusHours(2))
      insertTestAuditLog("Journal", "J001", "CREATE", "user2", "テストユーザー2", now.minusHours(1))
    }

    val startDate = now.minusDays(30).toInstant.toString
    val endDate = now.toInstant.toString

    Get(s"/api/audit-logs/action/CREATE?startDate=$startDate&endDate=$endDate") ~> auditLogRoutes.routes ~> check {
      status shouldBe StatusCodes.OK
      val jsonStr = responseAs[String]
      val json = jsonStr.parseJson.asInstanceOf[JsArray]
      json.elements should have length 2
      json.elements.foreach { elem =>
        elem.asJsObject.fields("action").convertTo[String] shouldBe "CREATE"
      }
    }
  }

  it should "存在しないアクションで空のリストを返す" in withContainers { container =>
    setupService(container)

    DB.localTx { implicit session =>
      clearTestData()
    }

    Get("/api/audit-logs/action/NONEXISTENT") ~> auditLogRoutes.routes ~> check {
      status shouldBe StatusCodes.OK
      val jsonStr = responseAs[String]
      val json = jsonStr.parseJson.asInstanceOf[JsArray]
      json.elements shouldBe empty
    }
  }
