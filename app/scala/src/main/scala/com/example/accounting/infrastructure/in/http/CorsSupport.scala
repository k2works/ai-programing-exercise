package com.example.accounting.infrastructure.in.http

import akka.http.scaladsl.model.HttpMethods.*
import akka.http.scaladsl.model.headers.*
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.{Directive0, Route}

/**
 * CORS サポート
 *
 * Script Lab や Excel Online からの API アクセスを許可するための CORS 設定
 */
object CorsSupport:

  // 許可するオリジン
  private val allowedOrigins: Seq[String] = Seq(
    // Script Lab
    "https://script-lab.azureedge.net",
    "https://script-lab-react.azureedge.net",
    "https://script-lab.public.cdn.office.net",
    // Office Online
    "https://excel.officeapps.live.com",
    "https://excel.office.com",
    "https://www.office.com",
    // Office CDN
    "https://appsforoffice.microsoft.com",
    // ローカル開発
    "http://localhost:3000",
    "http://localhost:5000",
    "http://localhost:5001",
    "http://localhost:8080",
    "http://127.0.0.1:3000",
    "http://127.0.0.1:5000",
    "http://127.0.0.1:8080",
    // null オリジン（ローカルファイルからのアクセス）
    "null"
  )

  // 許可するヘッダー
  private val allowedHeaders: Seq[String] = Seq(
    "Accept",
    "Accept-Encoding",
    "Accept-Language",
    "Authorization",
    "Content-Type",
    "Origin",
    "X-Requested-With"
  )

  // 許可するメソッド
  private val allowedMethods: Seq[akka.http.scaladsl.model.HttpMethod] = Seq(
    GET, POST, PUT, DELETE, OPTIONS, HEAD, PATCH
  )

  /**
   * CORS ヘッダーを追加するディレクティブ
   */
  def corsHandler(routes: Route): Route =
    optionalHeaderValueByType(`Origin`) {
      case Some(origin) =>
        val originValue = origin.value
        if isOriginAllowed(originValue) then
          respondWithHeaders(
            `Access-Control-Allow-Origin`(origin.value),
            `Access-Control-Allow-Credentials`(true),
            `Access-Control-Allow-Headers`(allowedHeaders.mkString(", ")),
            `Access-Control-Allow-Methods`(allowedMethods),
            `Access-Control-Max-Age`(86400) // 24時間キャッシュ
          ) {
            handlePreflightOrRoute(routes)
          }
        else
          // 許可されていないオリジンでも、開発環境では許可
          respondWithHeaders(
            `Access-Control-Allow-Origin`.*,
            `Access-Control-Allow-Credentials`(true),
            `Access-Control-Allow-Headers`(allowedHeaders.mkString(", ")),
            `Access-Control-Allow-Methods`(allowedMethods),
            `Access-Control-Max-Age`(86400)
          ) {
            handlePreflightOrRoute(routes)
          }

      case None =>
        // Origin ヘッダーがない場合（同一オリジンからのリクエスト）
        routes
    }

  /**
   * オリジンが許可されているかチェック
   */
  private def isOriginAllowed(origin: String): Boolean =
    allowedOrigins.contains(origin) ||
      origin.startsWith("http://localhost:") ||
      origin.startsWith("http://127.0.0.1:")

  /**
   * OPTIONS リクエスト（プリフライト）を処理
   */
  private def handlePreflightOrRoute(routes: Route): Route =
    extractRequest { request =>
      if request.method == OPTIONS then
        complete(HttpResponse(StatusCodes.OK))
      else
        routes
    }
