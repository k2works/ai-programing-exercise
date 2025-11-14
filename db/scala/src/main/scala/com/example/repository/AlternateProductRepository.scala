package com.example.repository

import com.example.domain.AlternateProduct
import scalikejdbc._

/**
 * 代替商品のリポジトリ
 */
trait AlternateProductRepository {
  def create(alternate: AlternateProduct)(implicit session: DBSession): Int
  def findById(prodCode: String, altProdCode: String)(implicit session: DBSession): Option[AlternateProduct]
  def findByProduct(prodCode: String)(implicit session: DBSession): List[AlternateProduct]
  def update(alternate: AlternateProduct)(implicit session: DBSession): Int
  def delete(prodCode: String, altProdCode: String)(implicit session: DBSession): Int
}

/**
 * AlternateProductRepository の実装
 */
class AlternateProductRepositoryImpl extends AlternateProductRepository {

  override def create(alternate: AlternateProduct)(implicit session: DBSession): Int = {
    sql"""
      INSERT INTO 代替商品 (
        商品コード, 代替商品コード, 優先順位,
        作成日時, 作成者名, 更新日時, 更新者名
      ) VALUES (
        ${alternate.prodCode}, ${alternate.altProdCode}, ${alternate.priority},
        ${alternate.createDate}, ${alternate.creator}, ${alternate.updateDate}, ${alternate.updater}
      )
    """.update.apply()
  }

  override def findById(prodCode: String, altProdCode: String)(implicit session: DBSession): Option[AlternateProduct] = {
    sql"""
      SELECT 商品コード, 代替商品コード, 優先順位,
             作成日時, 作成者名, 更新日時, 更新者名
      FROM 代替商品
      WHERE 商品コード = $prodCode AND 代替商品コード = $altProdCode
    """.map(AlternateProduct.apply).single.apply()
  }

  override def findByProduct(prodCode: String)(implicit session: DBSession): List[AlternateProduct] = {
    sql"""
      SELECT 商品コード, 代替商品コード, 優先順位,
             作成日時, 作成者名, 更新日時, 更新者名
      FROM 代替商品
      WHERE 商品コード = $prodCode
      ORDER BY 優先順位, 代替商品コード
    """.map(AlternateProduct.apply).list.apply()
  }

  override def update(alternate: AlternateProduct)(implicit session: DBSession): Int = {
    sql"""
      UPDATE 代替商品
      SET 優先順位 = ${alternate.priority},
          更新日時 = ${alternate.updateDate},
          更新者名 = ${alternate.updater}
      WHERE 商品コード = ${alternate.prodCode} AND 代替商品コード = ${alternate.altProdCode}
    """.update.apply()
  }

  override def delete(prodCode: String, altProdCode: String)(implicit session: DBSession): Int = {
    sql"""
      DELETE FROM 代替商品
      WHERE 商品コード = $prodCode AND 代替商品コード = $altProdCode
    """.update.apply()
  }
}

object AlternateProductRepository {
  def apply(): AlternateProductRepository = new AlternateProductRepositoryImpl()
}
