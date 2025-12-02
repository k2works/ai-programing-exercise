package com.example.accounting.infrastructure.in.http

import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, MediaTypes, StatusCodes}

/**
 * Swagger UI Routes
 *
 * WebJars から Swagger UI を提供し、OpenAPI 仕様を表示します。
 */
object SwaggerRoutes:

  private val swaggerUiVersion = "5.18.2"

  val routes: Route =
    concat(
      // Swagger UI ホームページ
      pathEndOrSingleSlash {
        redirect("/swagger-ui/index.html", StatusCodes.PermanentRedirect)
      },
      // OpenAPI 仕様ファイル
      path("openapi.yaml") {
        get {
          getFromResource("openapi.yaml")
        }
      },
      // Swagger UI 静的ファイル
      pathPrefix("swagger-ui") {
        concat(
          pathEndOrSingleSlash {
            redirect("/swagger-ui/index.html", StatusCodes.PermanentRedirect)
          },
          path("index.html") {
            get {
              complete(
                HttpEntity(
                  ContentTypes.`text/html(UTF-8)`,
                  swaggerUiHtml,
                )
              )
            }
          },
          // WebJars から静的ファイルを提供
          getFromResourceDirectory(s"META-INF/resources/webjars/swagger-ui/$swaggerUiVersion"),
        )
      },
    )

  private val swaggerUiHtml: String =
    s"""<!DOCTYPE html>
       |<html lang="ja">
       |<head>
       |  <meta charset="UTF-8">
       |  <meta name="viewport" content="width=device-width, initial-scale=1.0">
       |  <title>財務会計システム API - Swagger UI</title>
       |  <link rel="stylesheet" type="text/css" href="/swagger-ui/swagger-ui.css" />
       |  <link rel="icon" type="image/png" href="/swagger-ui/favicon-32x32.png" sizes="32x32" />
       |  <style>
       |    html {
       |      box-sizing: border-box;
       |      overflow-y: scroll;
       |    }
       |    *, *:before, *:after {
       |      box-sizing: inherit;
       |    }
       |    body {
       |      margin: 0;
       |      background: #fafafa;
       |    }
       |  </style>
       |</head>
       |<body>
       |  <div id="swagger-ui"></div>
       |  <script src="/swagger-ui/swagger-ui-bundle.js" charset="UTF-8"></script>
       |  <script src="/swagger-ui/swagger-ui-standalone-preset.js" charset="UTF-8"></script>
       |  <script>
       |    window.onload = function() {
       |      window.ui = SwaggerUIBundle({
       |        url: "/openapi.yaml",
       |        dom_id: '#swagger-ui',
       |        deepLinking: true,
       |        presets: [
       |          SwaggerUIBundle.presets.apis,
       |          SwaggerUIStandalonePreset
       |        ],
       |        plugins: [
       |          SwaggerUIBundle.plugins.DownloadUrl
       |        ],
       |        layout: "StandaloneLayout",
       |        validatorUrl: null,
       |        supportedSubmitMethods: ['get', 'post', 'put', 'delete', 'patch'],
       |        defaultModelsExpandDepth: 1,
       |        defaultModelExpandDepth: 1,
       |        docExpansion: 'list',
       |        filter: true,
       |        showExtensions: true,
       |        showCommonExtensions: true
       |      });
       |    };
       |  </script>
       |</body>
       |</html>
       |""".stripMargin
