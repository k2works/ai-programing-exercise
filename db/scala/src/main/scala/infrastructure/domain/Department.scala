package infrastructure.domain

import scalikejdbc._

import java.time.LocalDateTime

/**
 * 部門マスタのドメインモデル
 *
 * @param deptCode 部門コード
 * @param startDate 開始日
 * @param endDate 終了日
 * @param name 部門名
 * @param layer 組織階層
 * @param path 部門パス
 * @param lowestType 最下層区分
 * @param slitYn 伝票入力可否
 * @param createDate 作成日時
 * @param creator 作成者
 * @param updateDate 更新日時
 * @param updater 更新者
 */
case class Department(
  deptCode: String,
  startDate: LocalDateTime,
  endDate: LocalDateTime,
  name: String,
  layer: Int,
  path: String,
  lowestType: Int,
  slitYn: Int,
  createDate: LocalDateTime,
  creator: String,
  updateDate: LocalDateTime,
  updater: String
)

object Department extends SQLSyntaxSupport[Department] {
  override val tableName = "部門マスタ"
  override val columns = Seq(
    "部門コード", "開始日", "終了日", "部門名", "組織階層", "部門パス",
    "最下層区分", "伝票入力可否", "作成日時", "作成者名", "更新日時", "更新者名"
  )

  /**
   * ResultSetからDepartmentオブジェクトを生成
   */
  def apply(rs: WrappedResultSet): Department = Department(
    deptCode = rs.string("部門コード"),
    startDate = rs.timestamp("開始日").toLocalDateTime,
    endDate = rs.timestamp("終了日").toLocalDateTime,
    name = rs.string("部門名"),
    layer = rs.int("組織階層"),
    path = rs.string("部門パス"),
    lowestType = rs.int("最下層区分"),
    slitYn = rs.int("伝票入力可否"),
    createDate = rs.timestamp("作成日時").toLocalDateTime,
    creator = rs.string("作成者名"),
    updateDate = rs.timestamp("更新日時").toLocalDateTime,
    updater = rs.string("更新者名")
  )
}
