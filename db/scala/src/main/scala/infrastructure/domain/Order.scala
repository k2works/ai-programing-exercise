package infrastructure.domain

import scalikejdbc._

import java.time.LocalDateTime

/**
 * 受注データのドメインモデル
 *
 * 顧客からの受注全体の情報を保持する（ヘッダ）
 *
 * @param orderNo 受注番号
 * @param orderDate 受注日
 * @param deptCode 部門コード
 * @param startDate 開始日（部門の有効期間）
 * @param custCode 顧客コード
 * @param custSubNo 顧客枝番
 * @param empCode 担当社員コード
 * @param requiredDate 希望納期
 * @param custOrderNo 客先注文番号
 * @param whCode 倉庫コード
 * @param orderAmnt 受注金額合計
 * @param cmpTax 消費税合計
 * @param slipComment 備考
 * @param createDate 作成日時
 * @param creator 作成者
 * @param updateDate 更新日時
 * @param updater 更新者
 */
case class Order(
  orderNo: String,
  orderDate: LocalDateTime,
  deptCode: String,
  startDate: LocalDateTime,
  custCode: String,
  custSubNo: Int,
  empCode: String,
  requiredDate: Option[LocalDateTime] = None,
  custOrderNo: Option[String] = None,
  whCode: Option[String] = None,
  orderAmnt: Int = 0,
  cmpTax: Int = 0,
  slipComment: Option[String] = None,
  createDate: LocalDateTime = LocalDateTime.now(),
  creator: String = "",
  updateDate: LocalDateTime = LocalDateTime.now(),
  updater: String = ""
)

object Order extends SQLSyntaxSupport[Order] {
  override val tableName = "受注"
  override val columns = Seq(
    "受注番号", "受注日", "部門コード", "開始日", "顧客コード", "顧客枝番",
    "社員コード", "納期", "顧客注文番号", "倉庫コード", "受注金額", "消費税",
    "伝票コメント", "作成日時", "作成者名", "更新日時", "更新者名"
  )

  def apply(rs: WrappedResultSet): Order = Order(
    orderNo = rs.string("受注番号"),
    orderDate = rs.localDateTime("受注日"),
    deptCode = rs.string("部門コード"),
    startDate = rs.localDateTime("開始日"),
    custCode = rs.string("顧客コード"),
    custSubNo = rs.int("顧客枝番"),
    empCode = rs.string("社員コード"),
    requiredDate = rs.localDateTimeOpt("納期"),
    custOrderNo = rs.stringOpt("顧客注文番号"),
    whCode = rs.stringOpt("倉庫コード"),
    orderAmnt = rs.int("受注金額"),
    cmpTax = rs.int("消費税"),
    slipComment = rs.stringOpt("伝票コメント"),
    createDate = rs.localDateTime("作成日時"),
    creator = rs.string("作成者名"),
    updateDate = rs.localDateTime("更新日時"),
    updater = rs.string("更新者名")
  )
}
