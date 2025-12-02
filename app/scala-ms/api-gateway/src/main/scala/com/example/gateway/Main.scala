package com.example.gateway

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

/**
 * API Gateway メインエントリポイント
 *
 * すべてのクライアントリクエストのエントリポイントとして機能し、
 * 適切なバックエンドサービスにルーティングします。
 */
object Main extends IOApp:

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  override def run(args: List[String]): IO[ExitCode] =
    val serverPort = sys.env.get("SERVER_PORT").flatMap(_.toIntOption).getOrElse(8000)
    val financialServiceUrl = sys.env.getOrElse(
      "FINANCIAL_SERVICE_URL",
      "http://localhost:8080"
    )
    val managementServiceUrl = sys.env.getOrElse(
      "MANAGEMENT_SERVICE_URL",
      "http://localhost:8081"
    )

    // リソースの初期化と解放を管理
    val resources = for
      // HTTP クライアントの作成
      httpClient <- EmberClientBuilder.default[IO].build
      // HTTP サーバーの起動
      _ <- serverResource(serverPort, httpClient, financialServiceUrl, managementServiceUrl)
    yield ()

    resources.use(_ => IO.never).as(ExitCode.Success)

  private def serverResource(
      port: Int,
      httpClient: Client[IO],
      financialServiceUrl: String,
      managementServiceUrl: String
  ): Resource[IO, Unit] =
    Resource.eval(
      logger.info("Starting API Gateway...") >>
        logger.info(s"Financial Accounting Service: $financialServiceUrl") >>
        logger.info(s"Management Accounting Service: $managementServiceUrl")
    ) >>
      Resource.eval(IO {
        // ゲートウェイルートの作成
        val gatewayRoutes = GatewayRoutes(httpClient, financialServiceUrl, managementServiceUrl)
        gatewayRoutes
      }).flatMap { gatewayRoutes =>
        // ルーターの構成
        val allRoutes = Router(
          "/" -> gatewayRoutes.routes
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
          .withPort(Port.fromInt(port).getOrElse(port"8000"))
          .withHttpApp(httpApp)
          .build
          .evalTap { _ =>
            logger.info(s"API Gateway started on port $port") >>
              logger.info(s"Health check: http://localhost:$port/health")
          }
          .void
      }
