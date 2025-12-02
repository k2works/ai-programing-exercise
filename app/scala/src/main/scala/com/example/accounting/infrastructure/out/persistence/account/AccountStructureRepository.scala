package com.example.accounting.infrastructure.out.persistence.account

import com.example.accounting.domain.account.AccountStructure
import scalikejdbc.*

/**
 * 勘定科目構成マスタのリポジトリ
 */
class AccountStructureRepository:

  /**
   * 勘定科目構成を登録
   */
  def insert(structure: AccountStructure)(implicit session: DBSession): Int =
    sql"""
      INSERT INTO "勘定科目構成マスタ"
      ("勘定科目コード", "チルダパス", "階層レベル", "親科目コード")
      VALUES (
        ${structure.accountCode},
        ${structure.tildePath},
        ${structure.hierarchyLevel},
        ${structure.parentAccountCode}
      )
    """.update.apply()

  /**
   * 勘定科目コードで検索
   */
  def findByCode(code: String)(implicit session: DBSession): Option[AccountStructure] =
    sql"""
      SELECT * FROM "勘定科目構成マスタ"
      WHERE "勘定科目コード" = ${code}
    """.map(AccountStructure.apply).single.apply()

  /**
   * すべての勘定科目構成を取得
   */
  def findAll()(implicit session: DBSession): List[AccountStructure] =
    sql"""
      SELECT * FROM "勘定科目構成マスタ"
      ORDER BY "チルダパス"
    """.map(AccountStructure.apply).list.apply()

  /**
   * 親科目コードで子科目を検索
   */
  def findByParent(parentCode: String)(implicit session: DBSession): List[AccountStructure] =
    sql"""
      SELECT * FROM "勘定科目構成マスタ"
      WHERE "親科目コード" = ${parentCode}
      ORDER BY "勘定科目コード"
    """.map(AccountStructure.apply).list.apply()

  /**
   * チルダパスで子孫を検索（LIKE検索）
   */
  def findDescendants(parentPath: String)(implicit session: DBSession): List[AccountStructure] =
    val pattern = s"${parentPath}~%"
    sql"""
      SELECT * FROM "勘定科目構成マスタ"
      WHERE "チルダパス" LIKE ${pattern}
      ORDER BY "チルダパス"
    """.map(AccountStructure.apply).list.apply()

  /**
   * 階層レベルでフィルタリング
   */
  def findByLevel(level: Int)(implicit session: DBSession): List[AccountStructure] =
    sql"""
      SELECT * FROM "勘定科目構成マスタ"
      WHERE "階層レベル" = ${level}
      ORDER BY "チルダパス"
    """.map(AccountStructure.apply).list.apply()

  /**
   * 勘定科目構成を更新
   */
  def update(structure: AccountStructure)(implicit session: DBSession): Int =
    sql"""
      UPDATE "勘定科目構成マスタ"
      SET "チルダパス" = ${structure.tildePath},
          "階層レベル" = ${structure.hierarchyLevel},
          "親科目コード" = ${structure.parentAccountCode},
          "更新日時" = CURRENT_TIMESTAMP
      WHERE "勘定科目コード" = ${structure.accountCode}
    """.update.apply()

  /**
   * 勘定科目構成を削除
   */
  def delete(code: String)(implicit session: DBSession): Int =
    sql"""
      DELETE FROM "勘定科目構成マスタ"
      WHERE "勘定科目コード" = ${code}
    """.update.apply()
