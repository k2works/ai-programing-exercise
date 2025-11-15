package api

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.model.StatusCodes
import scalikejdbc._
import scalikejdbc.config._
import com.example.repository.ProductRepository
import api.service.ProductService
import api.presentation.ProductHandler
import api.support.JsonSupport
import api.schema.ErrorResponse
import api.swagger.SwaggerRoutes

import scala.concurrent.ExecutionContextExecutor
import scala.io.StdIn

object ApiServer extends JsonSupport {

  def main(args: Array[String]): Unit = {
    // Akka Actor System の作成
    implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "sales-api")
    implicit val executionContext: ExecutionContextExecutor = system.executionContext

    // ScalikeJDBC の初期化
    DBs.setupAll()

    // Repository 層の初期化
    val productRepo = new com.example.repository.ProductRepositoryImpl

    // Service 層の初期化
    val productService = new ProductService(productRepo)

    // Presentation 層の初期化
    val productHandler = new ProductHandler(productService)

    // ヘルスチェックエンドポイント
    val healthRoute: Route = path("health") {
      get {
        try {
          DB readOnly { implicit session =>
            // データベース接続確認
            sql"SELECT 1".map(_.int(1)).single.apply()
            complete(StatusCodes.OK, Map("status" -> "ok", "message" -> "Database connection is healthy"))
          }
        } catch {
          case e: Exception =>
            complete(StatusCodes.ServiceUnavailable, ErrorResponse("Database connection failed", Some(e.getMessage)))
        }
      }
    }

    // ルートの統合
    val routes: Route = concat(
      pathPrefix("api" / "v1") {
        productHandler.routes
      },
      SwaggerRoutes.routes,
      healthRoute
    )

    // サーバーの起動
    val bindingFuture = Http().newServerAt("0.0.0.0", 8080).bind(routes)

    println("🚀 Server started at http://0.0.0.0:8080/")
    println("📍 Endpoints:")
    println("  POST   /api/v1/products")
    println("  GET    /api/v1/products")
    println("  GET    /api/v1/products/:prodCode")
    println("  PUT    /api/v1/products/:prodCode")
    println("  DELETE /api/v1/products/:prodCode")
    println("  GET    /health")
    println()
    println("📖 API Documentation:")
    println("  Swagger UI: http://0.0.0.0:8080/api-docs")
    println("  OpenAPI Spec: http://0.0.0.0:8080/swagger.json")
    println()
    println("Press RETURN to stop...")

    StdIn.readLine()

    // サーバーの停止
    bindingFuture
      .flatMap(_.unbind())
      .onComplete { _ =>
        DBs.closeAll()
        system.terminate()
      }
  }
}
