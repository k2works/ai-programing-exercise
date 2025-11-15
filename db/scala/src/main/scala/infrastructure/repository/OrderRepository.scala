package infrastructure.repository

import infrastructure.entity.Order
import scalikejdbc._

/**
 * 受注データのリポジトリ
 */
trait OrderRepository {
  def create(order: Order)(implicit session: DBSession): Int
  def findById(orderNo: String)(implicit session: DBSession): Option[Order]
  def findAll()(implicit session: DBSession): List[Order]
  def findByCustomer(custCode: String, custSubNo: Int)(implicit session: DBSession): List[Order]
  def update(order: Order)(implicit session: DBSession): Int
  def delete(orderNo: String)(implicit session: DBSession): Int
}

/**
 * OrderRepository の実装
 */
class OrderRepositoryImpl extends OrderRepository {

  override def create(order: Order)(implicit session: DBSession): Int =
    sql"""
      INSERT INTO 受注 (
        受注番号, 受注日, 部門コード, 開始日, 顧客コード, 顧客枝番,
        社員コード, 納期, 顧客注文番号, 倉庫コード, 受注金額, 消費税,
        伝票コメント, 作成日時, 作成者名, 更新日時, 更新者名
      ) VALUES (
        ${order.orderNo}, ${order.orderDate}, ${order.deptCode}, ${order.startDate},
        ${order.custCode}, ${order.custSubNo}, ${order.empCode},
        ${order.requiredDate}, ${order.custOrderNo}, ${order.whCode},
        ${order.orderAmnt}, ${order.cmpTax}, ${order.slipComment},
        ${order.createDate}, ${order.creator}, ${order.updateDate}, ${order.updater}
      )
    """.update.apply()

  override def findById(orderNo: String)(implicit session: DBSession): Option[Order] =
    sql"""
      SELECT
        受注番号, 受注日, 部門コード, 開始日, 顧客コード, 顧客枝番,
        社員コード, 納期, 顧客注文番号, 倉庫コード, 受注金額, 消費税,
        伝票コメント, 作成日時, 作成者名, 更新日時, 更新者名
      FROM 受注
      WHERE 受注番号 = $orderNo
    """.map(Order.apply).single.apply()

  override def findAll()(implicit session: DBSession): List[Order] =
    sql"""
      SELECT
        受注番号, 受注日, 部門コード, 開始日, 顧客コード, 顧客枝番,
        社員コード, 納期, 顧客注文番号, 倉庫コード, 受注金額, 消費税,
        伝票コメント, 作成日時, 作成者名, 更新日時, 更新者名
      FROM 受注
      ORDER BY 受注日 DESC, 受注番号
    """.map(Order.apply).list.apply()

  override def findByCustomer(custCode: String, custSubNo: Int)(implicit
    session: DBSession
  ): List[Order] =
    sql"""
      SELECT
        受注番号, 受注日, 部門コード, 開始日, 顧客コード, 顧客枝番,
        社員コード, 納期, 顧客注文番号, 倉庫コード, 受注金額, 消費税,
        伝票コメント, 作成日時, 作成者名, 更新日時, 更新者名
      FROM 受注
      WHERE 顧客コード = $custCode AND 顧客枝番 = $custSubNo
      ORDER BY 受注日 DESC
    """.map(Order.apply).list.apply()

  override def update(order: Order)(implicit session: DBSession): Int =
    sql"""
      UPDATE 受注
      SET 受注日 = ${order.orderDate},
          納期 = ${order.requiredDate},
          受注金額 = ${order.orderAmnt},
          消費税 = ${order.cmpTax},
          伝票コメント = ${order.slipComment},
          更新日時 = ${order.updateDate},
          更新者名 = ${order.updater}
      WHERE 受注番号 = ${order.orderNo}
    """.update.apply()

  override def delete(orderNo: String)(implicit session: DBSession): Int =
    sql"""
      DELETE FROM 受注
      WHERE 受注番号 = $orderNo
    """.update.apply()

}

object OrderRepository {
  def apply(): OrderRepository = new OrderRepositoryImpl()
}
