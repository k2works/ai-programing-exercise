package com.example.domain

import scalikejdbc._

import java.time.LocalDateTime

/**
 * 取引先グループマスタのドメインモデル
 *
 * 取引先をグループ化して管理するためのマスタデータ
 *
 * @param compGroupCode 取引先グループコード
 * @param groupName 取引先グループ名
 * @param createDate 作成日時
 * @param creator 作成者
 * @param updateDate 更新日時
 * @param updater 更新者
 */
case class CompanyGroup(
  compGroupCode: String,
  groupName: String,
  createDate: LocalDateTime = LocalDateTime.now(),
  creator: String = "",
  updateDate: LocalDateTime = LocalDateTime.now(),
  updater: String = ""
)

object CompanyGroup extends SQLSyntaxSupport[CompanyGroup] {
  override val tableName = "取引先グループマスタ"
  override val columns = Seq(
    "取引先グループコード", "取引先グループ名",
    "作成日時", "作成者名", "更新日時", "更新者名"
  )

  def apply(rs: WrappedResultSet): CompanyGroup = CompanyGroup(
    compGroupCode = rs.string("取引先グループコード"),
    groupName = rs.string("取引先グループ名"),
    createDate = rs.localDateTime("作成日時"),
    creator = rs.string("作成者名"),
    updateDate = rs.localDateTime("更新日時"),
    updater = rs.string("更新者名")
  )
}
