package com.example.accounting.domain

import scalikejdbc.*
import java.time.LocalDateTime

/**
 * 勘定科目構成マスタのケースクラス
 */
case class AccountStructure(
    accountCode: String,
    tildePath: String,
    hierarchyLevel: Int,
    parentAccountCode: Option[String],
    createdAt: LocalDateTime,
    updatedAt: LocalDateTime,
):
  /**
   * チルダパスから親のパスを取得
   */
  def parentPath: Option[String] =
    val segments = tildePath.split("~")
    if segments.length > 1 then Some(segments.dropRight(1).mkString("~"))
    else None

  /**
   * チルダパスから子孫を検索するためのパターンを生成
   */
  def descendantPattern: String = s"${tildePath}~%"

object AccountStructure extends SQLSyntaxSupport[AccountStructure]:
  override val tableName = "勘定科目構成マスタ"
  override val columns = Seq(
    "勘定科目コード",
    "チルダパス",
    "階層レベル",
    "親科目コード",
    "作成日時",
    "更新日時",
  )

  def apply(rs: WrappedResultSet): AccountStructure = new AccountStructure(
    accountCode = rs.string("勘定科目コード"),
    tildePath = rs.string("チルダパス"),
    hierarchyLevel = rs.int("階層レベル"),
    parentAccountCode = rs.stringOpt("親科目コード"),
    createdAt = rs.localDateTime("作成日時"),
    updatedAt = rs.localDateTime("更新日時"),
  )
