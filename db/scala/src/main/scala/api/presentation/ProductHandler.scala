package api.presentation

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.model.StatusCodes
import scalikejdbc.{DB, DBSession}
import api.service.ProductService
import api.schema._
import api.support.JsonSupport
import io.circe.Encoder
import io.circe.syntax._

/**
 * 商品API のルート定義
 *
 * REST API エンドポイント:
 * - GET    /products        - 全商品取得
 * - GET    /products/:code  - 商品詳細取得
 * - POST   /products        - 商品作成
 * - PUT    /products/:code  - 商品更新
 * - DELETE /products/:code  - 商品削除
 */
class ProductHandler(service: ProductService) extends JsonSupport {

  val routes: Route = pathPrefix("products") {
    concat(
      // POST /products - 商品を作成
      pathEnd {
        post {
          entity(as[CreateProductRequest]) { request =>
            DB localTx { implicit session =>
              service.createProduct(request) match {
                case Right(product) =>
                  complete(StatusCodes.Created, product)
                case Left(error) =>
                  complete(StatusCodes.BadRequest, ErrorResponse(error))
              }
            }
          }
        }
      },
      // GET /products - すべての商品を取得
      pathEnd {
        get {
          DB readOnly { implicit session =>
            val products: List[ProductResponse] = service.getAllProducts()
            implicit val encoder: Encoder[List[ProductResponse]] = Encoder.encodeList[ProductResponse]
            complete(StatusCodes.OK, products)
          }
        }
      },
      // GET /products/:prodCode - ID で商品を取得
      path(Segment) { prodCode =>
        get {
          DB readOnly { implicit session =>
            service.getProductByCode(prodCode) match {
              case Some(product) =>
                complete(StatusCodes.OK, product)
              case None =>
                complete(StatusCodes.NotFound, ErrorResponse("商品が見つかりません"))
            }
          }
        }
      },
      // PUT /products/:prodCode - 商品を更新
      path(Segment) { prodCode =>
        put {
          entity(as[UpdateProductRequest]) { request =>
            DB localTx { implicit session =>
              service.updateProduct(prodCode, request) match {
                case Right(product) =>
                  complete(StatusCodes.OK, product)
                case Left(error) =>
                  if (error.contains("見つかりません")) {
                    complete(StatusCodes.NotFound, ErrorResponse(error))
                  } else {
                    complete(StatusCodes.BadRequest, ErrorResponse(error))
                  }
              }
            }
          }
        }
      },
      // DELETE /products/:prodCode - 商品を削除
      path(Segment) { prodCode =>
        delete {
          DB localTx { implicit session =>
            service.deleteProduct(prodCode) match {
              case Right(_) =>
                complete(StatusCodes.NoContent)
              case Left(error) =>
                complete(StatusCodes.NotFound, ErrorResponse(error))
            }
          }
        }
      }
    )
  }
}
