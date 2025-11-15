package api.schema

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto._
import infrastructure.entity.Product

// 商品作成リクエスト
case class CreateProductRequest(
  prodCode: String,
  fullName: String,
  name: String,
  kana: Option[String],
  unitPrice: Int,
  poPrice: Int,
  supCode: String,
  categoryCode: Option[String],
)

object CreateProductRequest {
  implicit val decoder: Decoder[CreateProductRequest] = deriveDecoder
  implicit val encoder: Encoder[CreateProductRequest] = deriveEncoder
}

// 商品更新リクエスト（すべてオプション）
case class UpdateProductRequest(
  fullName: Option[String] = None,
  name: Option[String] = None,
  kana: Option[String] = None,
  unitPrice: Option[Int] = None,
  poPrice: Option[Int] = None,
  supCode: Option[String] = None,
  categoryCode: Option[String] = None,
)

object UpdateProductRequest {
  implicit val decoder: Decoder[UpdateProductRequest] = deriveDecoder
  implicit val encoder: Encoder[UpdateProductRequest] = deriveEncoder
}

// 商品レスポンス
case class ProductResponse(
  prodCode: String,
  fullName: String,
  name: String,
  kana: Option[String],
  unitPrice: Int,
  poPrice: Int,
  supCode: String,
  categoryCode: Option[String],
)

object ProductResponse {
  implicit val decoder: Decoder[ProductResponse] = deriveDecoder
  implicit val encoder: Encoder[ProductResponse] = deriveEncoder

  // Domain モデルから変換
  def fromDomain(product: Product): ProductResponse =
    ProductResponse(
      prodCode = product.prodCode,
      fullName = product.fullName,
      name = product.name,
      kana = product.kana,
      unitPrice = product.unitPrice,
      poPrice = product.poPrice,
      supCode = product.supCode.getOrElse(""),
      categoryCode = product.categoryCode,
    )

}

// エラーレスポンス
case class ErrorResponse(error: String, details: Option[String] = None)

object ErrorResponse {
  implicit val decoder: Decoder[ErrorResponse] = deriveDecoder
  implicit val encoder: Encoder[ErrorResponse] = deriveEncoder
}
