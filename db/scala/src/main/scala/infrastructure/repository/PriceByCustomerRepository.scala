package infrastructure.repository

import infrastructure.entity.PriceByCustomer
import scalikejdbc._

/**
 * 顧客別販売単価のリポジトリ
 */
trait PriceByCustomerRepository {
  def create(price: PriceByCustomer)(implicit session: DBSession): Int

  def findById(prodCode: String, compCode: String)(implicit
    session: DBSession
  ): Option[PriceByCustomer]

  def findByProduct(prodCode: String)(implicit session: DBSession): List[PriceByCustomer]
  def findByCustomer(compCode: String)(implicit session: DBSession): List[PriceByCustomer]
  def update(price: PriceByCustomer)(implicit session: DBSession): Int
  def delete(prodCode: String, compCode: String)(implicit session: DBSession): Int
}

/**
 * PriceByCustomerRepository の実装
 */
class PriceByCustomerRepositoryImpl extends PriceByCustomerRepository {

  override def create(price: PriceByCustomer)(implicit session: DBSession): Int =
    sql"""
      INSERT INTO 顧客別販売単価 (
        商品コード, 取引先コード, 販売単価,
        作成日時, 作成者名, 更新日時, 更新者名
      ) VALUES (
        ${price.prodCode}, ${price.compCode}, ${price.unitPrice},
        ${price.createDate}, ${price.creator}, ${price.updateDate}, ${price.updater}
      )
    """.update.apply()

  override def findById(prodCode: String, compCode: String)(implicit
    session: DBSession
  ): Option[PriceByCustomer] =
    sql"""
      SELECT 商品コード, 取引先コード, 販売単価,
             作成日時, 作成者名, 更新日時, 更新者名
      FROM 顧客別販売単価
      WHERE 商品コード = $prodCode AND 取引先コード = $compCode
    """.map(PriceByCustomer.apply).single.apply()

  override def findByProduct(prodCode: String)(implicit session: DBSession): List[PriceByCustomer] =
    sql"""
      SELECT 商品コード, 取引先コード, 販売単価,
             作成日時, 作成者名, 更新日時, 更新者名
      FROM 顧客別販売単価
      WHERE 商品コード = $prodCode
      ORDER BY 取引先コード
    """.map(PriceByCustomer.apply).list.apply()

  override def findByCustomer(compCode: String)(implicit
    session: DBSession
  ): List[PriceByCustomer] =
    sql"""
      SELECT 商品コード, 取引先コード, 販売単価,
             作成日時, 作成者名, 更新日時, 更新者名
      FROM 顧客別販売単価
      WHERE 取引先コード = $compCode
      ORDER BY 商品コード
    """.map(PriceByCustomer.apply).list.apply()

  override def update(price: PriceByCustomer)(implicit session: DBSession): Int =
    sql"""
      UPDATE 顧客別販売単価
      SET 販売単価 = ${price.unitPrice},
          更新日時 = ${price.updateDate},
          更新者名 = ${price.updater}
      WHERE 商品コード = ${price.prodCode} AND 取引先コード = ${price.compCode}
    """.update.apply()

  override def delete(prodCode: String, compCode: String)(implicit session: DBSession): Int =
    sql"""
      DELETE FROM 顧客別販売単価
      WHERE 商品コード = $prodCode AND 取引先コード = $compCode
    """.update.apply()

}

object PriceByCustomerRepository {
  def apply(): PriceByCustomerRepository = new PriceByCustomerRepositoryImpl()
}
