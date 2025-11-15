package api.service

import scalikejdbc._
import infrastructure.domain.Product
import infrastructure.repository.ProductRepository
import api.schema._
import java.time.LocalDateTime

class ProductService(repo: ProductRepository) {

  def createProduct(request: CreateProductRequest)(implicit session: DBSession): Either[String, ProductResponse] = {
    // ビジネスルールの適用：単価が原価より安い場合はエラー
    if (request.unitPrice < request.poPrice) {
      Left("販売単価が仕入価格より低い設定はできません")
    } else {
      val now = LocalDateTime.now()
      val product = Product(
        prodCode = request.prodCode,
        fullName = request.fullName,
        name = request.name,
        kana = request.kana,
        unitPrice = request.unitPrice,
        poPrice = request.poPrice,
        supCode = Some(request.supCode),
        categoryCode = request.categoryCode,
        createDate = now,
        creator = "api",
        updateDate = now,
        updater = "api"
      )

      repo.create(product)
      repo.findById(request.prodCode) match {
        case Some(p) => Right(ProductResponse.fromDomain(p))
        case None => Left("商品の作成に失敗しました")
      }
    }
  }

  def getAllProducts()(implicit session: DBSession): List[ProductResponse] = {
    repo.findAll().map(ProductResponse.fromDomain)
  }

  def getProductByCode(prodCode: String)(implicit session: DBSession): Option[ProductResponse] = {
    repo.findById(prodCode).map(ProductResponse.fromDomain)
  }

  def updateProduct(prodCode: String, request: UpdateProductRequest)(implicit session: DBSession): Either[String, ProductResponse] = {
    repo.findById(prodCode) match {
      case Some(existing) =>
        val updated = existing.copy(
          fullName = request.fullName.getOrElse(existing.fullName),
          name = request.name.getOrElse(existing.name),
          kana = request.kana.orElse(existing.kana),
          unitPrice = request.unitPrice.getOrElse(existing.unitPrice),
          poPrice = request.poPrice.getOrElse(existing.poPrice),
          supCode = request.supCode.map(Some(_)).getOrElse(existing.supCode),
          categoryCode = request.categoryCode.orElse(existing.categoryCode),
          updateDate = LocalDateTime.now(),
          updater = "api"
        )

        // ビジネスルールの適用
        if (updated.unitPrice < updated.poPrice) {
          Left("販売単価が仕入価格より低い設定はできません")
        } else {
          repo.update(updated)
          Right(ProductResponse.fromDomain(updated))
        }

      case None =>
        Left("商品が見つかりません")
    }
  }

  def deleteProduct(prodCode: String)(implicit session: DBSession): Either[String, Unit] = {
    repo.findById(prodCode) match {
      case Some(_) =>
        repo.delete(prodCode)
        Right(())
      case None =>
        Left("商品が見つかりません")
    }
  }
}
