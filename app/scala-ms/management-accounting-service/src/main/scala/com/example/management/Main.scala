package com.example.management

import cats.effect.*
import cats.implicits.*
import com.comcast.ip4s.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.server.Router
import org.http4s.server.middleware.{CORS, Logger as Http4sLogger}
import org.http4s.client.Client
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import sttp.tapir.swagger.bundle.SwaggerInterpreter
import sttp.tapir.server.http4s.Http4sServerInterpreter

import com.example.management.application.FinancialAnalysisService
import com.example.management.adapter.api.AnalysisEndpoints
import com.example.management.adapter.client.FinancialAccountingClient

/**
 * 管理会計サービス メインエントリポイント
 *
 * Http4s + Tapir によるマイクロサービス
 * 財務会計サービスからデータを取得し、分析を行います
 */
object Main extends IOApp:

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  override def run(args: List[String]): IO[ExitCode] =
    val serverPort = sys.env.get("SERVER_PORT").flatMap(_.toIntOption).getOrElse(8081)
    val financialServiceUrl = sys.env.getOrElse(
      "FINANCIAL_SERVICE_URL",
      "http://localhost:8080"
    )

    // リソースの初期化と解放を管理
    val resources = for
      // HTTP クライアントの作成
      httpClient <- EmberClientBuilder.default[IO].build
      // HTTP サーバーの起動
      _ <- serverResource(serverPort, httpClient, financialServiceUrl)
    yield ()

    resources.use(_ => IO.never).as(ExitCode.Success)

  private def serverResource(
      port: Int,
      httpClient: Client[IO],
      financialServiceUrl: String
  ): Resource[IO, Unit] =
    Resource.eval(
      logger.info(s"Starting Management Accounting Service...") >>
        logger.info(s"Financial Accounting Service URL: $financialServiceUrl")
    ) >>
      Resource.eval(IO {
        // クライアントの作成（腐敗防止層）
        val financialClient = FinancialAccountingClient(httpClient, financialServiceUrl)

        // サービスの作成
        val analysisService = FinancialAnalysisService(financialClient)

        // エンドポイントの作成
        val analysisEndpoints = AnalysisEndpoints(analysisService)

        analysisEndpoints
      }).flatMap { analysisEndpoints =>
        // Swagger UI
        val swaggerRoutes = SwaggerInterpreter()
          .fromEndpoints[IO](analysisEndpoints.endpoints, "Management Accounting Service", "1.0.0")

        val swaggerHttpRoutes = Http4sServerInterpreter[IO]().toRoutes(swaggerRoutes)

        // ルーターの構成
        val allRoutes = Router(
          "/" -> (analysisEndpoints.routes <+> swaggerHttpRoutes)
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
          .withPort(Port.fromInt(port).getOrElse(port"8081"))
          .withHttpApp(httpApp)
          .build
          .evalTap { _ =>
            logger.info(s"Management Accounting Service started on port $port") >>
              logger.info(s"Swagger UI: http://localhost:$port/docs")
          }
          .void
      }
