package infrastructure.domain

import scalikejdbc._

import java.time.LocalDateTime

/**
 * 仕入データのドメインモデル
 *
 * 商品の仕入情報を保持する（ヘッダー）
 *
 * @param purchaseNo 仕入番号
 * @param purchaseDate 仕入日
 * @param poNo 発注番号
 * @param whCode 倉庫コード
 * @param completeFlg 完了フラグ（0:未完了、1:完了）
 * @param createDate 作成日時
 * @param creator 作成者
 * @param updateDate 更新日時
 * @param updater 更新者
 */
case class Purchase(
  purchaseNo: String,
  purchaseDate: LocalDateTime,
  poNo: String,
  whCode: String,
  completeFlg: Int = 0,
  createDate: LocalDateTime = LocalDateTime.now(),
  creator: String = "",
  updateDate: LocalDateTime = LocalDateTime.now(),
  updater: String = ""
)

object Purchase extends SQLSyntaxSupport[Purchase] {
  override val tableName = "仕入"
  override val columns = Seq(
    "仕入番号", "仕入日", "発注番号", "倉庫コード", "完了フラグ",
    "作成日時", "作成者名", "更新日時", "更新者名"
  )

  def apply(rs: WrappedResultSet): Purchase = Purchase(
    purchaseNo = rs.string("仕入番号"),
    purchaseDate = rs.localDateTime("仕入日"),
    poNo = rs.string("発注番号"),
    whCode = rs.string("倉庫コード"),
    completeFlg = rs.int("完了フラグ"),
    createDate = rs.localDateTime("作成日時"),
    creator = rs.string("作成者名"),
    updateDate = rs.localDateTime("更新日時"),
    updater = rs.string("更新者名")
  )
}

/**
 * 仕入明細のドメインモデル
 *
 * 仕入の明細情報を保持
 *
 * @param purchaseNo 仕入番号
 * @param purchaseDetailNo 仕入明細番号
 * @param prodCode 商品コード
 * @param rotNo ロット番号
 * @param qty 仕入数量
 * @param price 仕入単価
 * @param createDate 作成日時
 * @param creator 作成者
 * @param updateDate 更新日時
 * @param updater 更新者
 */
case class PurchaseDetail(
  purchaseNo: String,
  purchaseDetailNo: Int,
  prodCode: String,
  rotNo: String,
  qty: Int = 1,
  price: Int = 0,
  createDate: LocalDateTime = LocalDateTime.now(),
  creator: String = "",
  updateDate: LocalDateTime = LocalDateTime.now(),
  updater: String = ""
)

object PurchaseDetail extends SQLSyntaxSupport[PurchaseDetail] {
  override val tableName = "仕入明細"
  override val columns = Seq(
    "仕入番号", "仕入明細番号", "商品コード", "ロット番号", "数量", "単価",
    "作成日時", "作成者名", "更新日時", "更新者名"
  )

  def apply(rs: WrappedResultSet): PurchaseDetail = PurchaseDetail(
    purchaseNo = rs.string("仕入番号"),
    purchaseDetailNo = rs.int("仕入明細番号"),
    prodCode = rs.string("商品コード"),
    rotNo = rs.string("ロット番号"),
    qty = rs.int("数量"),
    price = rs.int("単価"),
    createDate = rs.localDateTime("作成日時"),
    creator = rs.string("作成者名"),
    updateDate = rs.localDateTime("更新日時"),
    updater = rs.string("更新者名")
  )
}
