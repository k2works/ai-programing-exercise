package infrastructure.domain

import scalikejdbc._
import java.time.LocalDateTime

/**
 * 売上データのドメインモデル
 *
 * 商品の売上情報を保持する（ヘッダー）
 *
 * @param salesNo 売上番号
 * @param salesDate 売上日
 * @param salesType 売上区分（1:通常売上、2:返品、3:値引）
 * @param orderNo 受注番号
 * @param deptCode 部門コード
 * @param startDate 部門の開始日
 * @param compCode 取引先コード
 * @param salesAmnt 売上金額
 * @param cmpTax 消費税
 * @param updatedNo 訂正番号（赤黒伝票）
 * @param originalNo 元伝票番号（赤黒伝票）
 * @param createDate 作成日時
 * @param creator 作成者名
 * @param updateDate 更新日時
 * @param updater 更新者名
 */
case class Sales(
  salesNo: String,
  salesDate: Option[LocalDateTime] = None,
  salesType: Int,
  orderNo: Option[String] = None,
  deptCode: String,
  startDate: LocalDateTime,
  compCode: String,
  salesAmnt: Int,
  cmpTax: Int,
  updatedNo: Option[String] = None,
  originalNo: Option[String] = None,
  createDate: LocalDateTime = LocalDateTime.now(),
  creator: Option[String] = None,
  updateDate: LocalDateTime = LocalDateTime.now(),
  updater: Option[String] = None
)

object Sales extends SQLSyntaxSupport[Sales] {
  override val tableName = "売上"
  override val columns = Seq(
    "売上番号", "売上日", "売上区分", "受注番号",
    "部門コード", "開始日", "取引先コード", "売上金額", "消費税",
    "訂正番号", "元伝票番号",
    "作成日時", "作成者名", "更新日時", "更新者名"
  )

  def apply(rs: WrappedResultSet): Sales = Sales(
    salesNo = rs.string("売上番号"),
    salesDate = rs.timestampOpt("売上日").map(_.toLocalDateTime),
    salesType = rs.int("売上区分"),
    orderNo = rs.stringOpt("受注番号"),
    deptCode = rs.string("部門コード"),
    startDate = rs.timestamp("開始日").toLocalDateTime,
    compCode = rs.string("取引先コード"),
    salesAmnt = rs.int("売上金額"),
    cmpTax = rs.int("消費税"),
    updatedNo = rs.stringOpt("訂正番号"),
    originalNo = rs.stringOpt("元伝票番号"),
    createDate = rs.timestamp("作成日時").toLocalDateTime,
    creator = rs.stringOpt("作成者名"),
    updateDate = rs.timestamp("更新日時").toLocalDateTime,
    updater = rs.stringOpt("更新者名")
  )
}

/**
 * 売上明細データのドメインモデル
 *
 * 売上の明細情報を保持する
 *
 * @param salesNo 売上番号
 * @param rowNo 明細番号
 * @param prodCode 商品コード
 * @param prodName 商品名
 * @param unitPrice 販売単価
 * @param deliveredQty 出荷済数量
 * @param qty 数量
 * @param discount 値引額
 * @param invoicedDate 請求日
 * @param invoiceNo 請求番号
 * @param invoiceDelayType 請求遅延区分
 * @param autoJournalDate 自動仕訳日
 * @param createDate 作成日時
 * @param creator 作成者名
 * @param updateDate 更新日時
 * @param updater 更新者名
 */
case class SalesDetail(
  salesNo: String,
  rowNo: Int,
  prodCode: String,
  prodName: Option[String] = None,
  unitPrice: Int,
  deliveredQty: Int,
  qty: Int,
  discount: Int,
  invoicedDate: Option[LocalDateTime] = None,
  invoiceNo: Option[String] = None,
  invoiceDelayType: Option[Int] = None,
  autoJournalDate: Option[LocalDateTime] = None,
  createDate: LocalDateTime = LocalDateTime.now(),
  creator: Option[String] = None,
  updateDate: LocalDateTime = LocalDateTime.now(),
  updater: Option[String] = None
)

object SalesDetail extends SQLSyntaxSupport[SalesDetail] {
  override val tableName = "売上明細"
  override val columns = Seq(
    "売上番号", "明細番号", "商品コード", "商品名", "販売単価",
    "出荷済数量", "数量", "値引額", "請求日", "請求番号",
    "請求遅延区分", "自動仕訳日",
    "作成日時", "作成者名", "更新日時", "更新者名"
  )

  def apply(rs: WrappedResultSet): SalesDetail = SalesDetail(
    salesNo = rs.string("売上番号"),
    rowNo = rs.int("明細番号"),
    prodCode = rs.string("商品コード"),
    prodName = rs.stringOpt("商品名"),
    unitPrice = rs.int("販売単価"),
    deliveredQty = rs.int("出荷済数量"),
    qty = rs.int("数量"),
    discount = rs.int("値引額"),
    invoicedDate = rs.timestampOpt("請求日").map(_.toLocalDateTime),
    invoiceNo = rs.stringOpt("請求番号"),
    invoiceDelayType = rs.intOpt("請求遅延区分"),
    autoJournalDate = rs.timestampOpt("自動仕訳日").map(_.toLocalDateTime),
    createDate = rs.timestamp("作成日時").toLocalDateTime,
    creator = rs.stringOpt("作成者名"),
    updateDate = rs.timestamp("更新日時").toLocalDateTime,
    updater = rs.stringOpt("更新者名")
  )
}
