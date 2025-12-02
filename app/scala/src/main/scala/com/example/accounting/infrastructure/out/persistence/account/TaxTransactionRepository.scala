package com.example.accounting.infrastructure.out.persistence.account

import com.example.accounting.domain.account.TaxTransaction
import scalikejdbc.*

/**
 * 課税取引マスタのリポジトリ
 */
class TaxTransactionRepository:

  /**
   * 課税取引を登録
   */
  def insert(tax: TaxTransaction)(implicit session: DBSession): Int =
    sql"""
      INSERT INTO "課税取引マスタ"
      ("課税取引コード", "課税取引名", "税率")
      VALUES (${tax.taxCode}, ${tax.taxName}, ${tax.taxRate})
    """.update.apply()

  /**
   * 課税取引コードで検索
   */
  def findByCode(code: String)(implicit session: DBSession): Option[TaxTransaction] =
    sql"""
      SELECT * FROM "課税取引マスタ"
      WHERE "課税取引コード" = ${code}
    """.map(TaxTransaction.apply).single.apply()

  /**
   * すべての課税取引を取得
   */
  def findAll()(implicit session: DBSession): List[TaxTransaction] =
    sql"""
      SELECT * FROM "課税取引マスタ"
      ORDER BY "課税取引コード"
    """.map(TaxTransaction.apply).list.apply()

  /**
   * 課税取引を更新
   */
  def update(tax: TaxTransaction)(implicit session: DBSession): Int =
    sql"""
      UPDATE "課税取引マスタ"
      SET "課税取引名" = ${tax.taxName},
          "税率" = ${tax.taxRate},
          "更新日時" = CURRENT_TIMESTAMP
      WHERE "課税取引コード" = ${tax.taxCode}
    """.update.apply()

  /**
   * 課税取引を削除
   */
  def delete(code: String)(implicit session: DBSession): Int =
    sql"""
      DELETE FROM "課税取引マスタ"
      WHERE "課税取引コード" = ${code}
    """.update.apply()
