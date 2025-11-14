package com.example.repository

import com.example.domain.ProductCategory
import scalikejdbc._

/**
 * 商品分類マスタのリポジトリ
 */
trait ProductCategoryRepository {
  def create(category: ProductCategory)(implicit session: DBSession): Int
  def findById(categoryCode: String)(implicit session: DBSession): Option[ProductCategory]
  def findAll()(implicit session: DBSession): List[ProductCategory]
  def findByPathPrefix(pathPrefix: String)(implicit session: DBSession): List[ProductCategory]
  def update(category: ProductCategory)(implicit session: DBSession): Int
  def delete(categoryCode: String)(implicit session: DBSession): Int
}

/**
 * ProductCategoryRepository の実装
 */
class ProductCategoryRepositoryImpl extends ProductCategoryRepository {

  override def create(category: ProductCategory)(implicit session: DBSession): Int = {
    sql"""
      INSERT INTO 商品分類マスタ (
        商品分類コード, 商品分類名, 商品分類階層, 商品分類パス, 最下層区分,
        作成日時, 作成者名, 更新日時, 更新者名
      ) VALUES (
        ${category.categoryCode}, ${category.name}, ${category.layer},
        ${category.path}, ${category.lowestType},
        ${category.createDate}, ${category.creator}, ${category.updateDate}, ${category.updater}
      )
    """.update.apply()
  }

  override def findById(categoryCode: String)(implicit session: DBSession): Option[ProductCategory] = {
    sql"""
      SELECT 商品分類コード, 商品分類名, 商品分類階層, 商品分類パス, 最下層区分,
             作成日時, 作成者名, 更新日時, 更新者名
      FROM 商品分類マスタ
      WHERE 商品分類コード = $categoryCode
    """.map(ProductCategory.apply).single.apply()
  }

  override def findAll()(implicit session: DBSession): List[ProductCategory] = {
    sql"""
      SELECT 商品分類コード, 商品分類名, 商品分類階層, 商品分類パス, 最下層区分,
             作成日時, 作成者名, 更新日時, 更新者名
      FROM 商品分類マスタ
      ORDER BY 商品分類パス
    """.map(ProductCategory.apply).list.apply()
  }

  override def findByPathPrefix(pathPrefix: String)(implicit session: DBSession): List[ProductCategory] = {
    sql"""
      SELECT 商品分類コード, 商品分類名, 商品分類階層, 商品分類パス, 最下層区分,
             作成日時, 作成者名, 更新日時, 更新者名
      FROM 商品分類マスタ
      WHERE 商品分類パス LIKE ${pathPrefix + "%"}
      ORDER BY 商品分類パス
    """.map(ProductCategory.apply).list.apply()
  }

  override def update(category: ProductCategory)(implicit session: DBSession): Int = {
    sql"""
      UPDATE 商品分類マスタ
      SET 商品分類名 = ${category.name},
          商品分類階層 = ${category.layer},
          商品分類パス = ${category.path},
          最下層区分 = ${category.lowestType},
          更新日時 = ${category.updateDate},
          更新者名 = ${category.updater}
      WHERE 商品分類コード = ${category.categoryCode}
    """.update.apply()
  }

  override def delete(categoryCode: String)(implicit session: DBSession): Int = {
    sql"""
      DELETE FROM 商品分類マスタ
      WHERE 商品分類コード = $categoryCode
    """.update.apply()
  }
}

object ProductCategoryRepository {
  def apply(): ProductCategoryRepository = new ProductCategoryRepositoryImpl()
}
