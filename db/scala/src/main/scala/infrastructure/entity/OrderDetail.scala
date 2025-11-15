package infrastructure.entity

import scalikejdbc._

import java.time.LocalDateTime

/**
 * 受注明細のドメインモデル
 *
 * 受注に含まれる各商品の情報を保持する（明細）
 *
 * @param orderNo 受注番号
 * @param soRowNo 受注行番号
 * @param prodCode 商品コード
 * @param prodName 商品名
 * @param unitPrice 販売単価
 * @param quantity 受注数量
 * @param cmpTaxRate 消費税率
 * @param reserveQty 引当数量
 * @param deliveryOrderQty 出荷指示数量
 * @param deliveredQty 出荷済数量
 * @param completeFlg 完了フラグ
 * @param discount 値引金額
 * @param deliveryDate 納期
 * @param createDate 作成日時
 * @param creator 作成者
 * @param updateDate 更新日時
 * @param updater 更新者
 */
case class OrderDetail(
  orderNo: String,
  soRowNo: Int,
  prodCode: String,
  prodName: String,
  unitPrice: Int = 0,
  quantity: Int = 1,
  cmpTaxRate: Int = 0,
  reserveQty: Int = 0,
  deliveryOrderQty: Int = 0,
  deliveredQty: Int = 0,
  completeFlg: Int = 0,
  discount: Int = 0,
  deliveryDate: Option[LocalDateTime] = None,
  createDate: LocalDateTime = LocalDateTime.now(),
  creator: String = "",
  updateDate: LocalDateTime = LocalDateTime.now(),
  updater: String = "",
)

object OrderDetail extends SQLSyntaxSupport[OrderDetail] {
  override val tableName = "受注明細"

  override val columns   = Seq(
    "受注番号",
    "明細番号",
    "商品コード",
    "商品名",
    "販売単価",
    "数量",
    "消費税率",
    "引当数量",
    "出荷指示数量",
    "出荷済数量",
    "完了フラグ",
    "値引額",
    "納品日",
    "作成日時",
    "作成者名",
    "更新日時",
    "更新者名",
  )

  def apply(rs: WrappedResultSet): OrderDetail = OrderDetail(
    orderNo = rs.string("受注番号"),
    soRowNo = rs.int("明細番号"),
    prodCode = rs.string("商品コード"),
    prodName = rs.string("商品名"),
    unitPrice = rs.int("販売単価"),
    quantity = rs.int("数量"),
    cmpTaxRate = rs.int("消費税率"),
    reserveQty = rs.int("引当数量"),
    deliveryOrderQty = rs.int("出荷指示数量"),
    deliveredQty = rs.int("出荷済数量"),
    completeFlg = rs.int("完了フラグ"),
    discount = rs.int("値引額"),
    deliveryDate = rs.localDateTimeOpt("納品日"),
    createDate = rs.localDateTime("作成日時"),
    creator = rs.string("作成者名"),
    updateDate = rs.localDateTime("更新日時"),
    updater = rs.string("更新者名"),
  )

}
