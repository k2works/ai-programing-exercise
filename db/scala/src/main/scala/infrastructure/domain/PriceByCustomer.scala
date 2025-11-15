package infrastructure.domain

import scalikejdbc._

import java.time.LocalDateTime

/**
 * 顧客別販売単価のドメインモデル
 *
 * 特定顧客向けの販売単価を管理する
 *
 * @param prodCode 商品コード
 * @param compCode 取引先コード
 * @param unitPrice 販売単価
 * @param createDate 作成日時
 * @param creator 作成者
 * @param updateDate 更新日時
 * @param updater 更新者
 */
case class PriceByCustomer(
  prodCode: String,
  compCode: String,
  unitPrice: Int,
  createDate: LocalDateTime = LocalDateTime.now(),
  creator: String = "",
  updateDate: LocalDateTime = LocalDateTime.now(),
  updater: String = ""
)

object PriceByCustomer extends SQLSyntaxSupport[PriceByCustomer] {
  override val tableName = "顧客別販売単価"
  override val columns = Seq(
    "商品コード", "取引先コード", "販売単価",
    "作成日時", "作成者名", "更新日時", "更新者名"
  )

  def apply(rs: WrappedResultSet): PriceByCustomer = PriceByCustomer(
    prodCode = rs.string("商品コード"),
    compCode = rs.string("取引先コード"),
    unitPrice = rs.int("販売単価"),
    createDate = rs.localDateTime("作成日時"),
    creator = rs.string("作成者名"),
    updateDate = rs.localDateTime("更新日時"),
    updater = rs.string("更新者名")
  )
}
