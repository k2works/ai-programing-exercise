package com.example.accounting.infrastructure.in.http

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import com.example.accounting.application.service.*
import com.example.accounting.infrastructure.out.persistence.account.AccountRepository
import com.example.accounting.infrastructure.out.persistence.audit.AuditLogRepository
import com.example.accounting.infrastructure.out.persistence.journal.JournalRepository
import scalikejdbc.config.DBs

import scala.concurrent.ExecutionContext
import scala.io.StdIn

/**
 * API サーバー
 */
object ApiServer:

  def main(args: Array[String]): Unit =
    // データベース接続の初期化
    DBs.setupAll()

    implicit val system: ActorSystem = ActorSystem("accounting-api")
    implicit val executionContext: ExecutionContext = system.dispatcher

    // リポジトリの初期化
    val accountRepository = AccountRepository()
    val journalRepository = JournalRepository()
    val auditLogRepository = AuditLogRepository()

    // サービスの初期化
    val accountService = AccountService(accountRepository)
    val journalService = JournalService(journalRepository, accountRepository)
    val financialStatementService = FinancialStatementService()
    val auditLogService = AuditLogService(auditLogRepository)

    // ルートの初期化
    val accountRoutes = AccountRoutes(accountService)
    val journalRoutes = JournalRoutes(journalService)
    val financialStatementRoutes = FinancialStatementRoutes(financialStatementService)
    val auditLogRoutes = AuditLogRoutes(auditLogService)

    // ルートの結合
    val routes: Route = concat(
      accountRoutes.routes,
      journalRoutes.routes,
      financialStatementRoutes.routes,
      auditLogRoutes.routes,
      SwaggerRoutes.routes,
    )

    // サーバーの起動
    val bindingFuture = Http().newServerAt("localhost", 8080).bind(routes)

    println("財務会計システム API サーバーが起動しました: http://localhost:8080/")
    println("Swagger UI: http://localhost:8080/swagger-ui/")
    println("終了するには ENTER を押してください...")
    StdIn.readLine()

    bindingFuture
      .flatMap(_.unbind())
      .onComplete { _ =>
        DBs.closeAll()
        system.terminate()
      }
