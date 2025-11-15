package infrastructure.repository

import infrastructure.domain.{Purchase, PurchaseDetail}
import scalikejdbc._

/**
 * 仕入データのリポジトリ
 */
trait PurchaseRepository {
  def create(purchase: Purchase)(implicit session: DBSession): Int
  def findByNo(purchaseNo: String)(implicit session: DBSession): Option[Purchase]
  def findAll()(implicit session: DBSession): List[Purchase]
  def findByPONo(poNo: String)(implicit session: DBSession): List[Purchase]
  def update(purchase: Purchase)(implicit session: DBSession): Int
  def delete(purchaseNo: String)(implicit session: DBSession): Int
}

/**
 * PurchaseRepository の実装
 */
class PurchaseRepositoryImpl extends PurchaseRepository {

  override def create(purchase: Purchase)(implicit session: DBSession): Int = {
    sql"""
      INSERT INTO 仕入 (
        仕入番号, 仕入日, 発注番号, 倉庫コード, 完了フラグ,
        作成日時, 作成者名, 更新日時, 更新者名
      ) VALUES (
        ${purchase.purchaseNo}, ${purchase.purchaseDate}, ${purchase.poNo},
        ${purchase.whCode}, ${purchase.completeFlg},
        ${purchase.createDate}, ${purchase.creator}, ${purchase.updateDate}, ${purchase.updater}
      )
    """.update.apply()
  }

  override def findByNo(purchaseNo: String)(implicit session: DBSession): Option[Purchase] = {
    sql"""
      SELECT
        仕入番号, 仕入日, 発注番号, 倉庫コード, 完了フラグ,
        作成日時, 作成者名, 更新日時, 更新者名
      FROM 仕入
      WHERE 仕入番号 = $purchaseNo
    """.map(Purchase.apply).single.apply()
  }

  override def findAll()(implicit session: DBSession): List[Purchase] = {
    sql"""
      SELECT
        仕入番号, 仕入日, 発注番号, 倉庫コード, 完了フラグ,
        作成日時, 作成者名, 更新日時, 更新者名
      FROM 仕入
      ORDER BY 仕入日 DESC
    """.map(Purchase.apply).list.apply()
  }

  override def findByPONo(poNo: String)(implicit session: DBSession): List[Purchase] = {
    sql"""
      SELECT
        仕入番号, 仕入日, 発注番号, 倉庫コード, 完了フラグ,
        作成日時, 作成者名, 更新日時, 更新者名
      FROM 仕入
      WHERE 発注番号 = $poNo
      ORDER BY 仕入日 DESC
    """.map(Purchase.apply).list.apply()
  }

  override def update(purchase: Purchase)(implicit session: DBSession): Int = {
    sql"""
      UPDATE 仕入
      SET 仕入日 = ${purchase.purchaseDate},
          完了フラグ = ${purchase.completeFlg},
          更新日時 = ${purchase.updateDate},
          更新者名 = ${purchase.updater}
      WHERE 仕入番号 = ${purchase.purchaseNo}
    """.update.apply()
  }

  override def delete(purchaseNo: String)(implicit session: DBSession): Int = {
    sql"""
      DELETE FROM 仕入 WHERE 仕入番号 = $purchaseNo
    """.update.apply()
  }
}

object PurchaseRepository {
  def apply(): PurchaseRepository = new PurchaseRepositoryImpl()
}

/**
 * 仕入明細のリポジトリ
 */
trait PurchaseDetailRepository {
  def create(detail: PurchaseDetail)(implicit session: DBSession): Int
  def findByPurchaseNo(purchaseNo: String)(implicit session: DBSession): List[PurchaseDetail]
  def update(detail: PurchaseDetail)(implicit session: DBSession): Int
  def delete(purchaseNo: String, detailNo: Int)(implicit session: DBSession): Int
}

/**
 * PurchaseDetailRepository の実装
 */
class PurchaseDetailRepositoryImpl extends PurchaseDetailRepository {

  override def create(detail: PurchaseDetail)(implicit session: DBSession): Int = {
    sql"""
      INSERT INTO 仕入明細 (
        仕入番号, 仕入明細番号, 商品コード, ロット番号, 数量, 単価,
        作成日時, 作成者名, 更新日時, 更新者名
      ) VALUES (
        ${detail.purchaseNo}, ${detail.purchaseDetailNo}, ${detail.prodCode},
        ${detail.rotNo}, ${detail.qty}, ${detail.price},
        ${detail.createDate}, ${detail.creator}, ${detail.updateDate}, ${detail.updater}
      )
    """.update.apply()
  }

  override def findByPurchaseNo(purchaseNo: String)(implicit session: DBSession): List[PurchaseDetail] = {
    sql"""
      SELECT
        仕入番号, 仕入明細番号, 商品コード, ロット番号, 数量, 単価,
        作成日時, 作成者名, 更新日時, 更新者名
      FROM 仕入明細
      WHERE 仕入番号 = $purchaseNo
      ORDER BY 仕入明細番号
    """.map(PurchaseDetail.apply).list.apply()
  }

  override def update(detail: PurchaseDetail)(implicit session: DBSession): Int = {
    sql"""
      UPDATE 仕入明細
      SET 数量 = ${detail.qty},
          単価 = ${detail.price},
          更新日時 = ${detail.updateDate},
          更新者名 = ${detail.updater}
      WHERE 仕入番号 = ${detail.purchaseNo} AND 仕入明細番号 = ${detail.purchaseDetailNo}
    """.update.apply()
  }

  override def delete(purchaseNo: String, detailNo: Int)(implicit session: DBSession): Int = {
    sql"""
      DELETE FROM 仕入明細
      WHERE 仕入番号 = $purchaseNo AND 仕入明細番号 = $detailNo
    """.update.apply()
  }
}

object PurchaseDetailRepository {
  def apply(): PurchaseDetailRepository = new PurchaseDetailRepositoryImpl()
}
