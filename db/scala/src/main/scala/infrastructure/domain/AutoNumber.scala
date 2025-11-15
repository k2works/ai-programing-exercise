package infrastructure.domain

import scalikejdbc._
import java.time.LocalDateTime

/**
 * 自動採番データのドメインモデル
 *
 * 伝票番号を自動採番するための情報を保持する
 *
 * @param slipType 伝票種別（OR:受注、SL:売上、IV:請求など）
 * @param yearMonth 採番の対象となる年月
 * @param lastSlipNo 最後に採番された番号
 */
case class AutoNumber(
  slipType: String,
  yearMonth: LocalDateTime,
  lastSlipNo: Int
)

object AutoNumber extends SQLSyntaxSupport[AutoNumber] {
  override val tableName = "自動採番"
  override val columns = Seq("伝票種別", "年月", "最終伝票番号")

  def apply(rs: WrappedResultSet): AutoNumber = AutoNumber(
    slipType = rs.string("伝票種別"),
    yearMonth = rs.timestamp("年月").toLocalDateTime,
    lastSlipNo = rs.int("最終伝票番号")
  )
}
