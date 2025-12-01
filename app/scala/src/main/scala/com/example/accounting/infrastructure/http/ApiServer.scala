package com.example.accounting.infrastructure.http

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import com.example.accounting.application.*
import com.example.accounting.infrastructure.*
import com.example.accounting.infrastructure.journal.JournalRepository
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

    // サービスの初期化
    val accountService = AccountService(accountRepository)
    val journalService = JournalService(journalRepository, accountRepository)
    val financialStatementService = FinancialStatementService()

    // ルートの初期化
    val accountRoutes = AccountRoutes(accountService)
    val journalRoutes = JournalRoutes(journalService)
    val financialStatementRoutes = FinancialStatementRoutes(financialStatementService)

    // ルートの結合
    val routes: Route = concat(
      accountRoutes.routes,
      journalRoutes.routes,
      financialStatementRoutes.routes,
    )

    // サーバーの起動
    val bindingFuture = Http().newServerAt("localhost", 8080).bind(routes)

    println("財務会計システム API サーバーが起動しました: http://localhost:8080/")
    println("終了するには ENTER を押してください...")
    StdIn.readLine()

    bindingFuture
      .flatMap(_.unbind())
      .onComplete { _ =>
        DBs.closeAll()
        system.terminate()
      }
