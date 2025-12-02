package com.example.gateway

import cats.effect.IO
import cats.implicits.*
import org.http4s.*
import org.http4s.dsl.io.*
import org.http4s.headers.Location
import org.http4s.client.Client
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import fs2.Chunk

/**
 * API Gateway ルーティング
 *
 * クライアントからのリクエストを適切なバックエンドサービスにルーティングします。
 */
class GatewayRoutes(
    httpClient: Client[IO],
    financialServiceUrl: String,
    managementServiceUrl: String
):
  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {

    // ヘルスチェック
    case GET -> Root / "health" =>
      Ok("OK")

    // 財務会計サービスへのルーティング
    case req @ (GET | POST | PUT | DELETE) -> Root / "api" / "v1" / "accounts" =>
      proxyTo(req, financialServiceUrl)

    case req @ (GET | POST | PUT | DELETE) -> Root / "api" / "v1" / "accounts" / _ =>
      proxyTo(req, financialServiceUrl)

    case req @ (GET | POST | DELETE) -> Root / "api" / "v1" / "journals" =>
      proxyTo(req, financialServiceUrl)

    case req @ (GET | DELETE) -> Root / "api" / "v1" / "journals" / _ =>
      proxyTo(req, financialServiceUrl)

    // 管理会計サービスへのルーティング（具体的なパスを先に定義）
    case req @ GET -> Root / "api" / "v1" / "financial-analysis" / "compare" =>
      proxyTo(req, managementServiceUrl)

    case req @ GET -> Root / "api" / "v1" / "financial-analysis" / "data" / _ =>
      proxyTo(req, managementServiceUrl)

    case req @ GET -> Root / "api" / "v1" / "financial-analysis" / _ =>
      proxyTo(req, managementServiceUrl)

    // Swagger UI へのルーティング（各サービス）
    case req @ GET -> Root / "docs" / "financial" =>
      redirectTo(s"$financialServiceUrl/docs")

    case req @ GET -> Root / "docs" / "management" =>
      redirectTo(s"$managementServiceUrl/docs")

    // デフォルト: 404
    case _ =>
      NotFound("Endpoint not found")
  }

  /**
   * リクエストをバックエンドサービスにプロキシ
   */
  private def proxyTo(req: Request[IO], targetBaseUrl: String): IO[Response[IO]] =
    val targetUri = Uri
      .unsafeFromString(targetBaseUrl)
      .withPath(req.uri.path)
      .withMultiValueQueryParams(req.uri.query.multiParams)

    val targetRequest = req
      .withUri(targetUri)
      .removeHeader[headers.Host]

    logger.debug(s"Proxying ${req.method} ${req.uri} -> $targetUri") >>
      httpClient
        .run(targetRequest)
        .use { response =>
          // レスポンスボディを読み取ってから返す
          response.body.compile.to(Chunk).flatMap { body =>
            IO.pure(
              Response[IO](
                status = response.status,
                headers = response.headers,
                body = fs2.Stream.chunk(body)
              )
            )
          }
        }
        .handleErrorWith { error =>
          logger.error(error)(s"Proxy error: ${error.getMessage}") >>
            InternalServerError(s"Service unavailable: ${error.getMessage}")
        }

  /**
   * リダイレクト
   */
  private def redirectTo(url: String): IO[Response[IO]] =
    Found(Location(Uri.unsafeFromString(url)))
