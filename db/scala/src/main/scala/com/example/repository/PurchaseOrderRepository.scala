package com.example.repository

import com.example.domain.{PurchaseOrder, PurchaseOrderDetail}
import scalikejdbc._

/**
 * 発注データのリポジトリ
 */
trait PurchaseOrderRepository {
  def create(po: PurchaseOrder)(implicit session: DBSession): Int
  def findByNo(poNo: String)(implicit session: DBSession): Option[PurchaseOrder]
  def findAll()(implicit session: DBSession): List[PurchaseOrder]
  def findBySupplier(supCode: String, supSubNo: Int)(implicit session: DBSession): List[PurchaseOrder]
  def update(po: PurchaseOrder)(implicit session: DBSession): Int
  def delete(poNo: String)(implicit session: DBSession): Int
}

/**
 * PurchaseOrderRepository の実装
 */
class PurchaseOrderRepositoryImpl extends PurchaseOrderRepository {

  override def create(po: PurchaseOrder)(implicit session: DBSession): Int = {
    sql"""
      INSERT INTO 発注 (
        発注番号, 発注日, 部門コード, 開始日, 仕入先コード, 仕入先枝番,
        社員コード, 完了フラグ, 作成日時, 作成者名, 更新日時, 更新者名
      ) VALUES (
        ${po.poNo}, ${po.poDate}, ${po.deptCode}, ${po.startDate},
        ${po.supCode}, ${po.supSubNo}, ${po.empCode}, ${po.completeFlg},
        ${po.createDate}, ${po.creator}, ${po.updateDate}, ${po.updater}
      )
    """.update.apply()
  }

  override def findByNo(poNo: String)(implicit session: DBSession): Option[PurchaseOrder] = {
    sql"""
      SELECT
        発注番号, 発注日, 部門コード, 開始日, 仕入先コード, 仕入先枝番,
        社員コード, 完了フラグ, 作成日時, 作成者名, 更新日時, 更新者名
      FROM 発注
      WHERE 発注番号 = $poNo
    """.map(PurchaseOrder.apply).single.apply()
  }

  override def findAll()(implicit session: DBSession): List[PurchaseOrder] = {
    sql"""
      SELECT
        発注番号, 発注日, 部門コード, 開始日, 仕入先コード, 仕入先枝番,
        社員コード, 完了フラグ, 作成日時, 作成者名, 更新日時, 更新者名
      FROM 発注
      ORDER BY 発注日 DESC
    """.map(PurchaseOrder.apply).list.apply()
  }

  override def findBySupplier(supCode: String, supSubNo: Int)(implicit session: DBSession): List[PurchaseOrder] = {
    sql"""
      SELECT
        発注番号, 発注日, 部門コード, 開始日, 仕入先コード, 仕入先枝番,
        社員コード, 完了フラグ, 作成日時, 作成者名, 更新日時, 更新者名
      FROM 発注
      WHERE 仕入先コード = $supCode AND 仕入先枝番 = $supSubNo
      ORDER BY 発注日 DESC
    """.map(PurchaseOrder.apply).list.apply()
  }

  override def update(po: PurchaseOrder)(implicit session: DBSession): Int = {
    sql"""
      UPDATE 発注
      SET 発注日 = ${po.poDate},
          完了フラグ = ${po.completeFlg},
          更新日時 = ${po.updateDate},
          更新者名 = ${po.updater}
      WHERE 発注番号 = ${po.poNo}
    """.update.apply()
  }

  override def delete(poNo: String)(implicit session: DBSession): Int = {
    sql"""
      DELETE FROM 発注 WHERE 発注番号 = $poNo
    """.update.apply()
  }
}

object PurchaseOrderRepository {
  def apply(): PurchaseOrderRepository = new PurchaseOrderRepositoryImpl()
}

/**
 * 発注明細のリポジトリ
 */
trait PurchaseOrderDetailRepository {
  def create(detail: PurchaseOrderDetail)(implicit session: DBSession): Int
  def findByPONo(poNo: String)(implicit session: DBSession): List[PurchaseOrderDetail]
  def update(detail: PurchaseOrderDetail)(implicit session: DBSession): Int
  def delete(poNo: String, detailNo: Int)(implicit session: DBSession): Int
}

/**
 * PurchaseOrderDetailRepository の実装
 */
class PurchaseOrderDetailRepositoryImpl extends PurchaseOrderDetailRepository {

  override def create(detail: PurchaseOrderDetail)(implicit session: DBSession): Int = {
    sql"""
      INSERT INTO 発注明細 (
        発注番号, 発注明細番号, 商品コード, 数量, 単価,
        作成日時, 作成者名, 更新日時, 更新者名
      ) VALUES (
        ${detail.poNo}, ${detail.poDetailNo}, ${detail.prodCode},
        ${detail.qty}, ${detail.price},
        ${detail.createDate}, ${detail.creator}, ${detail.updateDate}, ${detail.updater}
      )
    """.update.apply()
  }

  override def findByPONo(poNo: String)(implicit session: DBSession): List[PurchaseOrderDetail] = {
    sql"""
      SELECT
        発注番号, 発注明細番号, 商品コード, 数量, 単価,
        作成日時, 作成者名, 更新日時, 更新者名
      FROM 発注明細
      WHERE 発注番号 = $poNo
      ORDER BY 発注明細番号
    """.map(PurchaseOrderDetail.apply).list.apply()
  }

  override def update(detail: PurchaseOrderDetail)(implicit session: DBSession): Int = {
    sql"""
      UPDATE 発注明細
      SET 数量 = ${detail.qty},
          単価 = ${detail.price},
          更新日時 = ${detail.updateDate},
          更新者名 = ${detail.updater}
      WHERE 発注番号 = ${detail.poNo} AND 発注明細番号 = ${detail.poDetailNo}
    """.update.apply()
  }

  override def delete(poNo: String, detailNo: Int)(implicit session: DBSession): Int = {
    sql"""
      DELETE FROM 発注明細
      WHERE 発注番号 = $poNo AND 発注明細番号 = $detailNo
    """.update.apply()
  }
}

object PurchaseOrderDetailRepository {
  def apply(): PurchaseOrderDetailRepository = new PurchaseOrderDetailRepositoryImpl()
}
