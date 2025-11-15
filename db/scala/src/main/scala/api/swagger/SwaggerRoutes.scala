package api.swagger

import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import io.circe.syntax._
import io.circe.Json

/**
 * Swagger UI のルート定義
 *
 * WebJars 経由で Swagger UI を配信し、OpenAPI 仕様を提供
 */
object SwaggerRoutes {

  /**
   * OpenAPI 3.0 仕様を JSON で定義
   */
  private val openApiSpec: Json = Json.obj(
    "openapi" -> Json.fromString("3.0.0"),
    "info" -> Json.obj(
      "title" -> Json.fromString("販売管理システム API"),
      "description" -> Json.fromString("ScalikeJDBC + Akka HTTP による販売管理システムの REST API"),
      "version" -> Json.fromString("1.0.0")
    ),
    "servers" -> Json.arr(
      Json.obj(
        "url" -> Json.fromString("http://localhost:8080"),
        "description" -> Json.fromString("開発環境")
      )
    ),
    "paths" -> Json.obj(
      "/api/v1/products" -> Json.obj(
        "get" -> Json.obj(
          "summary" -> Json.fromString("商品一覧取得"),
          "description" -> Json.fromString("すべての商品を取得します"),
          "tags" -> Json.arr(Json.fromString("Products")),
          "responses" -> Json.obj(
            "200" -> Json.obj(
              "description" -> Json.fromString("成功"),
              "content" -> Json.obj(
                "application/json" -> Json.obj(
                  "schema" -> Json.obj(
                    "type" -> Json.fromString("array"),
                    "items" -> Json.obj("$ref" -> Json.fromString("#/components/schemas/ProductResponse"))
                  )
                )
              )
            )
          )
        ),
        "post" -> Json.obj(
          "summary" -> Json.fromString("商品作成"),
          "description" -> Json.fromString("新しい商品を作成します"),
          "tags" -> Json.arr(Json.fromString("Products")),
          "requestBody" -> Json.obj(
            "required" -> Json.fromBoolean(true),
            "content" -> Json.obj(
              "application/json" -> Json.obj(
                "schema" -> Json.obj("$ref" -> Json.fromString("#/components/schemas/CreateProductRequest"))
              )
            )
          ),
          "responses" -> Json.obj(
            "201" -> Json.obj(
              "description" -> Json.fromString("作成成功"),
              "content" -> Json.obj(
                "application/json" -> Json.obj(
                  "schema" -> Json.obj("$ref" -> Json.fromString("#/components/schemas/ProductResponse"))
                )
              )
            ),
            "400" -> Json.obj(
              "description" -> Json.fromString("バリデーションエラー"),
              "content" -> Json.obj(
                "application/json" -> Json.obj(
                  "schema" -> Json.obj("$ref" -> Json.fromString("#/components/schemas/ErrorResponse"))
                )
              )
            )
          )
        )
      ),
      "/api/v1/products/{prodCode}" -> Json.obj(
        "get" -> Json.obj(
          "summary" -> Json.fromString("商品詳細取得"),
          "description" -> Json.fromString("指定された商品コードの商品を取得します"),
          "tags" -> Json.arr(Json.fromString("Products")),
          "parameters" -> Json.arr(
            Json.obj(
              "name" -> Json.fromString("prodCode"),
              "in" -> Json.fromString("path"),
              "required" -> Json.fromBoolean(true),
              "schema" -> Json.obj("type" -> Json.fromString("string")),
              "description" -> Json.fromString("商品コード")
            )
          ),
          "responses" -> Json.obj(
            "200" -> Json.obj(
              "description" -> Json.fromString("成功"),
              "content" -> Json.obj(
                "application/json" -> Json.obj(
                  "schema" -> Json.obj("$ref" -> Json.fromString("#/components/schemas/ProductResponse"))
                )
              )
            ),
            "404" -> Json.obj(
              "description" -> Json.fromString("商品が見つかりません"),
              "content" -> Json.obj(
                "application/json" -> Json.obj(
                  "schema" -> Json.obj("$ref" -> Json.fromString("#/components/schemas/ErrorResponse"))
                )
              )
            )
          )
        ),
        "put" -> Json.obj(
          "summary" -> Json.fromString("商品更新"),
          "description" -> Json.fromString("指定された商品コードの商品を更新します"),
          "tags" -> Json.arr(Json.fromString("Products")),
          "parameters" -> Json.arr(
            Json.obj(
              "name" -> Json.fromString("prodCode"),
              "in" -> Json.fromString("path"),
              "required" -> Json.fromBoolean(true),
              "schema" -> Json.obj("type" -> Json.fromString("string")),
              "description" -> Json.fromString("商品コード")
            )
          ),
          "requestBody" -> Json.obj(
            "required" -> Json.fromBoolean(true),
            "content" -> Json.obj(
              "application/json" -> Json.obj(
                "schema" -> Json.obj("$ref" -> Json.fromString("#/components/schemas/UpdateProductRequest"))
              )
            )
          ),
          "responses" -> Json.obj(
            "200" -> Json.obj(
              "description" -> Json.fromString("更新成功"),
              "content" -> Json.obj(
                "application/json" -> Json.obj(
                  "schema" -> Json.obj("$ref" -> Json.fromString("#/components/schemas/ProductResponse"))
                )
              )
            ),
            "404" -> Json.obj(
              "description" -> Json.fromString("商品が見つかりません"),
              "content" -> Json.obj(
                "application/json" -> Json.obj(
                  "schema" -> Json.obj("$ref" -> Json.fromString("#/components/schemas/ErrorResponse"))
                )
              )
            )
          )
        ),
        "delete" -> Json.obj(
          "summary" -> Json.fromString("商品削除"),
          "description" -> Json.fromString("指定された商品コードの商品を削除します"),
          "tags" -> Json.arr(Json.fromString("Products")),
          "parameters" -> Json.arr(
            Json.obj(
              "name" -> Json.fromString("prodCode"),
              "in" -> Json.fromString("path"),
              "required" -> Json.fromBoolean(true),
              "schema" -> Json.obj("type" -> Json.fromString("string")),
              "description" -> Json.fromString("商品コード")
            )
          ),
          "responses" -> Json.obj(
            "204" -> Json.obj(
              "description" -> Json.fromString("削除成功")
            ),
            "404" -> Json.obj(
              "description" -> Json.fromString("商品が見つかりません"),
              "content" -> Json.obj(
                "application/json" -> Json.obj(
                  "schema" -> Json.obj("$ref" -> Json.fromString("#/components/schemas/ErrorResponse"))
                )
              )
            )
          )
        )
      ),
      "/health" -> Json.obj(
        "get" -> Json.obj(
          "summary" -> Json.fromString("ヘルスチェック"),
          "description" -> Json.fromString("API サーバーとデータベースの接続状態を確認します"),
          "tags" -> Json.arr(Json.fromString("Health")),
          "responses" -> Json.obj(
            "200" -> Json.obj(
              "description" -> Json.fromString("正常"),
              "content" -> Json.obj(
                "application/json" -> Json.obj(
                  "schema" -> Json.obj(
                    "type" -> Json.fromString("object"),
                    "properties" -> Json.obj(
                      "status" -> Json.obj("type" -> Json.fromString("string")),
                      "message" -> Json.obj("type" -> Json.fromString("string"))
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    "components" -> Json.obj(
      "schemas" -> Json.obj(
        "ProductResponse" -> Json.obj(
          "type" -> Json.fromString("object"),
          "required" -> Json.arr(
            Json.fromString("prodCode"),
            Json.fromString("fullName"),
            Json.fromString("name"),
            Json.fromString("unitPrice"),
            Json.fromString("poPrice"),
            Json.fromString("supCode")
          ),
          "properties" -> Json.obj(
            "prodCode" -> Json.obj("type" -> Json.fromString("string"), "description" -> Json.fromString("商品コード")),
            "fullName" -> Json.obj("type" -> Json.fromString("string"), "description" -> Json.fromString("商品正式名")),
            "name" -> Json.obj("type" -> Json.fromString("string"), "description" -> Json.fromString("商品略称")),
            "kana" -> Json.obj("type" -> Json.fromString("string"), "nullable" -> Json.fromBoolean(true), "description" -> Json.fromString("商品名カナ")),
            "unitPrice" -> Json.obj("type" -> Json.fromString("integer"), "description" -> Json.fromString("販売単価")),
            "poPrice" -> Json.obj("type" -> Json.fromString("integer"), "description" -> Json.fromString("仕入単価")),
            "supCode" -> Json.obj("type" -> Json.fromString("string"), "description" -> Json.fromString("仕入先コード")),
            "categoryCode" -> Json.obj("type" -> Json.fromString("string"), "nullable" -> Json.fromBoolean(true), "description" -> Json.fromString("商品分類コード"))
          )
        ),
        "CreateProductRequest" -> Json.obj(
          "type" -> Json.fromString("object"),
          "required" -> Json.arr(
            Json.fromString("prodCode"),
            Json.fromString("fullName"),
            Json.fromString("name"),
            Json.fromString("unitPrice"),
            Json.fromString("poPrice"),
            Json.fromString("supCode")
          ),
          "properties" -> Json.obj(
            "prodCode" -> Json.obj("type" -> Json.fromString("string"), "description" -> Json.fromString("商品コード")),
            "fullName" -> Json.obj("type" -> Json.fromString("string"), "description" -> Json.fromString("商品正式名")),
            "name" -> Json.obj("type" -> Json.fromString("string"), "description" -> Json.fromString("商品略称")),
            "kana" -> Json.obj("type" -> Json.fromString("string"), "nullable" -> Json.fromBoolean(true), "description" -> Json.fromString("商品名カナ")),
            "unitPrice" -> Json.obj("type" -> Json.fromString("integer"), "description" -> Json.fromString("販売単価")),
            "poPrice" -> Json.obj("type" -> Json.fromString("integer"), "description" -> Json.fromString("仕入単価")),
            "supCode" -> Json.obj("type" -> Json.fromString("string"), "description" -> Json.fromString("仕入先コード")),
            "categoryCode" -> Json.obj("type" -> Json.fromString("string"), "nullable" -> Json.fromBoolean(true), "description" -> Json.fromString("商品分類コード"))
          )
        ),
        "UpdateProductRequest" -> Json.obj(
          "type" -> Json.fromString("object"),
          "properties" -> Json.obj(
            "fullName" -> Json.obj("type" -> Json.fromString("string"), "description" -> Json.fromString("商品正式名")),
            "name" -> Json.obj("type" -> Json.fromString("string"), "description" -> Json.fromString("商品略称")),
            "kana" -> Json.obj("type" -> Json.fromString("string"), "nullable" -> Json.fromBoolean(true), "description" -> Json.fromString("商品名カナ")),
            "unitPrice" -> Json.obj("type" -> Json.fromString("integer"), "description" -> Json.fromString("販売単価")),
            "poPrice" -> Json.obj("type" -> Json.fromString("integer"), "description" -> Json.fromString("仕入単価")),
            "supCode" -> Json.obj("type" -> Json.fromString("string"), "description" -> Json.fromString("仕入先コード")),
            "categoryCode" -> Json.obj("type" -> Json.fromString("string"), "nullable" -> Json.fromBoolean(true), "description" -> Json.fromString("商品分類コード"))
          )
        ),
        "ErrorResponse" -> Json.obj(
          "type" -> Json.fromString("object"),
          "required" -> Json.arr(Json.fromString("error")),
          "properties" -> Json.obj(
            "error" -> Json.obj("type" -> Json.fromString("string"), "description" -> Json.fromString("エラーメッセージ")),
            "details" -> Json.obj("type" -> Json.fromString("string"), "nullable" -> Json.fromBoolean(true), "description" -> Json.fromString("エラー詳細"))
          )
        )
      )
    )
  )

  /**
   * OpenAPI 仕様を提供するルート（再利用可能）
   */
  val openApiRoute: Route = get {
    complete(HttpEntity(ContentTypes.`application/json`, openApiSpec.spaces2))
  }

  /**
   * Swagger UI ルート
   */
  val routes: Route = concat(
    // OpenAPI 仕様を JSON で提供（ルート直下）
    path("swagger.json") {
      openApiRoute
    },
    // OpenAPI 仕様を JSON で提供（API バージョン下）
    pathPrefix("api" / "v1") {
      path("openapi.json") {
        openApiRoute
      }
    },
    // Swagger UI（カスタム設定ファイルを優先）
    pathPrefix("swagger-ui") {
      concat(
        // カスタム swagger-initializer.js を配信
        path("swagger-initializer.js") {
          getFromResource("swagger-ui/swagger-initializer.js")
        },
        // その他のファイルは WebJars から配信
        getFromResourceDirectory("META-INF/resources/webjars/swagger-ui/5.10.3")
      )
    },
    // Swagger UI のインデックスページへリダイレクト
    path("api-docs") {
      redirect("/swagger-ui/index.html", akka.http.scaladsl.model.StatusCodes.PermanentRedirect)
    },
    // API バージョン固有の Swagger UI
    pathPrefix("api" / "v1") {
      path("docs") {
        redirect("/swagger-ui/index.html", akka.http.scaladsl.model.StatusCodes.PermanentRedirect)
      }
    }
  )
}
