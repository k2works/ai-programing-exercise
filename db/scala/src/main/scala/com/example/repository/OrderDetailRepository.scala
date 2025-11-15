package com.example.repository

import com.example.domain.OrderDetail
import scalikejdbc._

/**
 * 受注明細のリポジトリ
 */
trait OrderDetailRepository {
  def create(detail: OrderDetail)(implicit session: DBSession): Int
  def findByOrderNo(orderNo: String)(implicit session: DBSession): List[OrderDetail]
  def findById(orderNo: String, soRowNo: Int)(implicit session: DBSession): Option[OrderDetail]
  def update(detail: OrderDetail)(implicit session: DBSession): Int
  def delete(orderNo: String, soRowNo: Int)(implicit session: DBSession): Int
}

/**
 * OrderDetailRepository の実装
 */
class OrderDetailRepositoryImpl extends OrderDetailRepository {

  override def create(detail: OrderDetail)(implicit session: DBSession): Int = {
    sql"""
      INSERT INTO 受注明細 (
        受注番号, 明細番号, 商品コード, 商品名, 販売単価, 数量,
        消費税率, 引当数量, 出荷指示数量, 出荷済数量, 完了フラグ,
        値引額, 納品日, 作成日時, 作成者名, 更新日時, 更新者名
      ) VALUES (
        ${detail.orderNo}, ${detail.soRowNo}, ${detail.prodCode}, ${detail.prodName},
        ${detail.unitPrice}, ${detail.quantity}, ${detail.cmpTaxRate},
        ${detail.reserveQty}, ${detail.deliveryOrderQty}, ${detail.deliveredQty},
        ${detail.completeFlg}, ${detail.discount}, ${detail.deliveryDate},
        ${detail.createDate}, ${detail.creator}, ${detail.updateDate}, ${detail.updater}
      )
    """.update.apply()
  }

  override def findByOrderNo(orderNo: String)(implicit session: DBSession): List[OrderDetail] = {
    sql"""
      SELECT
        受注番号, 明細番号, 商品コード, 商品名, 販売単価, 数量,
        消費税率, 引当数量, 出荷指示数量, 出荷済数量, 完了フラグ,
        値引額, 納品日, 作成日時, 作成者名, 更新日時, 更新者名
      FROM 受注明細
      WHERE 受注番号 = $orderNo
      ORDER BY 明細番号
    """.map(OrderDetail.apply).list.apply()
  }

  override def findById(orderNo: String, soRowNo: Int)(implicit session: DBSession): Option[OrderDetail] = {
    sql"""
      SELECT
        受注番号, 明細番号, 商品コード, 商品名, 販売単価, 数量,
        消費税率, 引当数量, 出荷指示数量, 出荷済数量, 完了フラグ,
        値引額, 納品日, 作成日時, 作成者名, 更新日時, 更新者名
      FROM 受注明細
      WHERE 受注番号 = $orderNo AND 明細番号 = $soRowNo
    """.map(OrderDetail.apply).single.apply()
  }

  override def update(detail: OrderDetail)(implicit session: DBSession): Int = {
    sql"""
      UPDATE 受注明細
      SET 数量 = ${detail.quantity},
          引当数量 = ${detail.reserveQty},
          出荷指示数量 = ${detail.deliveryOrderQty},
          出荷済数量 = ${detail.deliveredQty},
          完了フラグ = ${detail.completeFlg},
          値引額 = ${detail.discount},
          納品日 = ${detail.deliveryDate},
          更新日時 = ${detail.updateDate},
          更新者名 = ${detail.updater}
      WHERE 受注番号 = ${detail.orderNo} AND 明細番号 = ${detail.soRowNo}
    """.update.apply()
  }

  override def delete(orderNo: String, soRowNo: Int)(implicit session: DBSession): Int = {
    sql"""
      DELETE FROM 受注明細
      WHERE 受注番号 = $orderNo AND 明細番号 = $soRowNo
    """.update.apply()
  }
}

object OrderDetailRepository {
  def apply(): OrderDetailRepository = new OrderDetailRepositoryImpl()
}
