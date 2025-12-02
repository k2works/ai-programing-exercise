package com.example.financial

import cats.effect.*
import cats.implicits.*
import com.comcast.ip4s.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Router
import org.http4s.server.middleware.{CORS, Logger as Http4sLogger}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import sttp.tapir.swagger.bundle.SwaggerInterpreter
import sttp.tapir.server.http4s.Http4sServerInterpreter

import com.example.financial.config.DatabaseConfig
import com.example.financial.application.*
import com.example.financial.adapter.api.*
import com.example.financial.adapter.repository.*

/**
 * 財務会計サービス メインエントリポイント
 *
 * Http4s + Tapir + ScalikeJDBC によるマイクロサービス
 */
object Main extends IOApp:

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  override def run(args: List[String]): IO[ExitCode] =
    val dbConfig = DatabaseConfig.loadFromEnv()
    val serverPort = sys.env.get("SERVER_PORT").flatMap(_.toIntOption).getOrElse(8080)

    // リソースの初期化と解放を管理
    val resources = for
      // データベース接続の初期化
      _ <- DatabaseConfig.initialize(dbConfig)
      // HTTP サーバーの起動
      _ <- serverResource(serverPort, dbConfig)
    yield ()

    resources.use(_ => IO.never).as(ExitCode.Success)

  private def serverResource(port: Int, dbConfig: DatabaseConfig.Config): Resource[IO, Unit] =
    Resource.eval(
      for
        _ <- logger.info("Starting Financial Accounting Service...")
        // マイグレーション実行
        migrated <- DatabaseConfig.migrate(dbConfig)
        _ <- logger.info(s"Database migration completed: $migrated migrations executed")
      yield ()
    ) >>
      Resource.eval(IO {
        // リポジトリの作成
        val accountRepository = AccountRepositoryImpl()
        val journalRepository = JournalRepositoryImpl()
        val balanceRepository = BalanceRepositoryImpl()

        // サービスの作成
        val accountService = AccountService(accountRepository)
        val journalService = JournalService(journalRepository, balanceRepository, accountRepository)

        // エンドポイントの作成
        val accountEndpoints = AccountEndpoints(accountService)
        val journalEndpoints = JournalEndpoints(journalService)

        (accountEndpoints, journalEndpoints)
      }).flatMap { case (accountEndpoints, journalEndpoints) =>
        // Swagger UI
        val swaggerEndpoints = accountEndpoints.endpoints ++ journalEndpoints.endpoints
        val swaggerRoutes = SwaggerInterpreter()
          .fromEndpoints[IO](swaggerEndpoints, "Financial Accounting Service", "1.0.0")

        val swaggerHttpRoutes = Http4sServerInterpreter[IO]().toRoutes(swaggerRoutes)

        // ルーターの構成
        val allRoutes = Router(
          "/" -> (accountEndpoints.routes <+> journalEndpoints.routes <+> swaggerHttpRoutes)
        ).orNotFound

        // CORS とロギングミドルウェアを適用
        val corsConfig = CORS.policy
          .withAllowOriginAll
          .withAllowMethodsAll
          .withAllowHeadersAll

        val httpApp = corsConfig(Http4sLogger.httpApp(
          logHeaders = true,
          logBody = false
        )(allRoutes))

        // サーバーの起動
        EmberServerBuilder
          .default[IO]
          .withHost(ipv4"0.0.0.0")
          .withPort(Port.fromInt(port).getOrElse(port"8080"))
          .withHttpApp(httpApp)
          .build
          .evalTap { _ =>
            logger.info(s"Financial Accounting Service started on port $port") >>
              logger.info(s"Swagger UI: http://localhost:$port/docs")
          }
          .void
      }
