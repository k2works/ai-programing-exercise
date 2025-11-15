package infrastructure.repository

import infrastructure.domain.Product
import scalikejdbc._

/**
 * 商品マスタのリポジトリ
 */
trait ProductRepository {
  def create(product: Product)(implicit session: DBSession): Int
  def findById(prodCode: String)(implicit session: DBSession): Option[Product]
  def findAll()(implicit session: DBSession): List[Product]
  def findByCategory(categoryCode: String)(implicit session: DBSession): List[Product]
  def update(product: Product)(implicit session: DBSession): Int
  def delete(prodCode: String)(implicit session: DBSession): Int
}

/**
 * ProductRepository の実装
 */
class ProductRepositoryImpl extends ProductRepository {

  override def create(product: Product)(implicit session: DBSession): Int = {
    sql"""
      INSERT INTO 商品マスタ (
        商品コード, 商品正式名, 商品略称, 商品名カナ, 商品区分, 製品型番,
        販売単価, 仕入単価, 売上原価, 税区分,
        商品分類コード, 雑区分, 在庫管理対象区分, 在庫引当区分,
        仕入先コード, 仕入先枝番,
        作成日時, 作成者名, 更新日時, 更新者名
      ) VALUES (
        ${product.prodCode}, ${product.fullName}, ${product.name}, ${product.kana},
        ${product.prodType}, ${product.serialNo},
        ${product.unitPrice}, ${product.poPrice}, ${product.primeCost}, ${product.taxType},
        ${product.categoryCode}, ${product.wideUseType}, ${product.stockManageType},
        ${product.stockReserveType}, ${product.supCode}, ${product.supSubNo},
        ${product.createDate}, ${product.creator}, ${product.updateDate}, ${product.updater}
      )
    """.update.apply()
  }

  override def findById(prodCode: String)(implicit session: DBSession): Option[Product] = {
    sql"""
      SELECT 商品コード, 商品正式名, 商品略称, 商品名カナ, 商品区分, 製品型番,
             販売単価, 仕入単価, 売上原価, 税区分,
             商品分類コード, 雑区分, 在庫管理対象区分, 在庫引当区分,
             仕入先コード, 仕入先枝番,
             作成日時, 作成者名, 更新日時, 更新者名
      FROM 商品マスタ
      WHERE 商品コード = $prodCode
    """.map(Product.apply).single.apply()
  }

  override def findAll()(implicit session: DBSession): List[Product] = {
    sql"""
      SELECT 商品コード, 商品正式名, 商品略称, 商品名カナ, 商品区分, 製品型番,
             販売単価, 仕入単価, 売上原価, 税区分,
             商品分類コード, 雑区分, 在庫管理対象区分, 在庫引当区分,
             仕入先コード, 仕入先枝番,
             作成日時, 作成者名, 更新日時, 更新者名
      FROM 商品マスタ
      ORDER BY 商品コード
    """.map(Product.apply).list.apply()
  }

  override def findByCategory(categoryCode: String)(implicit session: DBSession): List[Product] = {
    sql"""
      SELECT 商品コード, 商品正式名, 商品略称, 商品名カナ, 商品区分, 製品型番,
             販売単価, 仕入単価, 売上原価, 税区分,
             商品分類コード, 雑区分, 在庫管理対象区分, 在庫引当区分,
             仕入先コード, 仕入先枝番,
             作成日時, 作成者名, 更新日時, 更新者名
      FROM 商品マスタ
      WHERE 商品分類コード = $categoryCode
      ORDER BY 商品コード
    """.map(Product.apply).list.apply()
  }

  override def update(product: Product)(implicit session: DBSession): Int = {
    sql"""
      UPDATE 商品マスタ
      SET 商品正式名 = ${product.fullName},
          商品略称 = ${product.name},
          商品名カナ = ${product.kana},
          商品区分 = ${product.prodType},
          製品型番 = ${product.serialNo},
          販売単価 = ${product.unitPrice},
          仕入単価 = ${product.poPrice},
          売上原価 = ${product.primeCost},
          税区分 = ${product.taxType},
          商品分類コード = ${product.categoryCode},
          雑区分 = ${product.wideUseType},
          在庫管理対象区分 = ${product.stockManageType},
          在庫引当区分 = ${product.stockReserveType},
          仕入先コード = ${product.supCode},
          仕入先枝番 = ${product.supSubNo},
          更新日時 = ${product.updateDate},
          更新者名 = ${product.updater}
      WHERE 商品コード = ${product.prodCode}
    """.update.apply()
  }

  override def delete(prodCode: String)(implicit session: DBSession): Int = {
    sql"""
      DELETE FROM 商品マスタ
      WHERE 商品コード = $prodCode
    """.update.apply()
  }
}

object ProductRepository {
  def apply(): ProductRepository = new ProductRepositoryImpl()
}
