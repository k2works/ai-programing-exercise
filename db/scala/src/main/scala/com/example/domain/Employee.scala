package com.example.domain

import scalikejdbc._

import java.time.LocalDateTime

/**
 * 社員マスタのドメインモデル
 *
 * @param empCode 社員コード
 * @param name 社員名
 * @param kana 社員名カナ
 * @param loginPassword パスワード
 * @param tel 電話番号
 * @param fax FAX番号
 * @param deptCode 部門コード（外部キー）
 * @param startDate 開始日
 * @param occuCode 職種コード
 * @param approvalCode 承認権限コード
 * @param createDate 作成日時
 * @param creator 作成者名
 * @param updateDate 更新日時
 * @param updater 更新者名
 */
case class Employee(
  empCode: String,
  name: String,
  kana: String,
  loginPassword: String,
  tel: String,
  fax: String,
  deptCode: String,      // 外部キー
  startDate: LocalDateTime,
  occuCode: String,
  approvalCode: String,
  createDate: LocalDateTime,
  creator: String,
  updateDate: LocalDateTime,
  updater: String
)

object Employee extends SQLSyntaxSupport[Employee] {
  override val tableName = "社員マスタ"
  override val columns = Seq(
    "社員コード", "社員名", "社員名カナ", "パスワード",
    "電話番号", "FAX番号", "部門コード", "開始日",
    "職種コード", "承認権限コード",
    "作成日時", "作成者名", "更新日時", "更新者名"
  )

  /**
   * ResultSetからEmployeeオブジェクトを生成
   */
  def apply(rs: WrappedResultSet): Employee = Employee(
    empCode = rs.string("社員コード"),
    name = rs.string("社員名"),
    kana = rs.string("社員名カナ"),
    loginPassword = rs.string("パスワード"),
    tel = rs.string("電話番号"),
    fax = rs.string("FAX番号"),
    deptCode = rs.string("部門コード"),
    startDate = rs.timestamp("開始日").toLocalDateTime,
    occuCode = rs.string("職種コード"),
    approvalCode = rs.string("承認権限コード"),
    createDate = rs.timestamp("作成日時").toLocalDateTime,
    creator = rs.string("作成者名"),
    updateDate = rs.timestamp("更新日時").toLocalDateTime,
    updater = rs.string("更新者名")
  )
}
