package com.example.accounting.infrastructure.persistence

import com.example.accounting.domain.{Account, AccountType}
import scalikejdbc.*

/**
 * 勘定科目マスタのリポジトリ
 */
class AccountRepository:

  private def fromResultSet(rs: WrappedResultSet): Account = Account(
    accountId = Some(rs.int("勘定科目ID")),
    accountCode = rs.string("勘定科目コード"),
    accountName = rs.string("勘定科目名"),
    accountType = AccountType.fromString(rs.string("勘定科目種別")),
    balance = rs.bigDecimal("残高"),
    bsplDistinction = rs.stringOpt("BSPL区分"),
    transactionElement = rs.stringOpt("取引要素区分"),
    expenseDistinction = rs.stringOpt("費用区分"),
    isSummaryAccount = rs.boolean("合計科目"),
    displayOrder = rs.intOpt("表示順序"),
    isAggregationTarget = rs.boolean("集計対象"),
    accountKana = rs.stringOpt("勘定科目カナ"),
    taxCode = rs.stringOpt("課税取引コード"),
    createdAt = rs.localDateTime("作成日時"),
    updatedAt = rs.localDateTime("更新日時"),
  )

  /**
   * 勘定科目を登録
   */
  def insert(account: Account)(implicit session: DBSession): Int =
    sql"""
      INSERT INTO "勘定科目マスタ"
      ("勘定科目コード", "勘定科目名", "勘定科目種別", "残高",
       "BSPL区分", "取引要素区分", "費用区分", "合計科目",
       "表示順序", "集計対象", "勘定科目カナ", "課税取引コード")
      VALUES (
        ${account.accountCode},
        ${account.accountName},
        ${account.accountType.value}::account_type,
        ${account.balance},
        ${account.bsplDistinction},
        ${account.transactionElement},
        ${account.expenseDistinction},
        ${account.isSummaryAccount},
        ${account.displayOrder},
        ${account.isAggregationTarget},
        ${account.accountKana},
        ${account.taxCode}
      )
    """.update.apply()

  /**
   * 勘定科目IDで検索
   */
  def findById(id: Int)(implicit session: DBSession): Option[Account] =
    sql"""
      SELECT * FROM "勘定科目マスタ"
      WHERE "勘定科目ID" = ${id}
    """.map(fromResultSet).single.apply()

  /**
   * 勘定科目コードで検索
   */
  def findByCode(code: String)(implicit session: DBSession): Option[Account] =
    sql"""
      SELECT * FROM "勘定科目マスタ"
      WHERE "勘定科目コード" = ${code}
    """.map(fromResultSet).single.apply()

  /**
   * すべての勘定科目を取得
   */
  def findAll()(implicit session: DBSession): List[Account] =
    sql"""
      SELECT * FROM "勘定科目マスタ"
      ORDER BY "勘定科目コード"
    """.map(fromResultSet).list.apply()

  /**
   * 勘定科目種別で検索
   */
  def findByType(accountType: AccountType)(implicit session: DBSession): List[Account] =
    sql"""
      SELECT * FROM "勘定科目マスタ"
      WHERE "勘定科目種別" = ${accountType.value}::account_type
      ORDER BY "表示順序", "勘定科目コード"
    """.map(fromResultSet).list.apply()

  /**
   * BSPL区分で検索
   */
  def findByBsplDistinction(distinction: String)(implicit session: DBSession): List[Account] =
    sql"""
      SELECT * FROM "勘定科目マスタ"
      WHERE "BSPL区分" = ${distinction}
      ORDER BY "表示順序", "勘定科目コード"
    """.map(fromResultSet).list.apply()

  /**
   * 集計対象の勘定科目を取得
   */
  def findAggregationTargets()(implicit session: DBSession): List[Account] =
    sql"""
      SELECT * FROM "勘定科目マスタ"
      WHERE "集計対象" = true
      ORDER BY "表示順序", "勘定科目コード"
    """.map(fromResultSet).list.apply()

  /**
   * 合計科目を取得
   */
  def findSummaryAccounts()(implicit session: DBSession): List[Account] =
    sql"""
      SELECT * FROM "勘定科目マスタ"
      WHERE "合計科目" = true
      ORDER BY "表示順序", "勘定科目コード"
    """.map(fromResultSet).list.apply()

  /**
   * 課税取引コードで検索
   */
  def findByTaxCode(taxCode: String)(implicit session: DBSession): List[Account] =
    sql"""
      SELECT * FROM "勘定科目マスタ"
      WHERE "課税取引コード" = ${taxCode}
      ORDER BY "勘定科目コード"
    """.map(fromResultSet).list.apply()

  /**
   * 勘定科目カナで前方一致検索
   */
  def findByKanaPrefix(kana: String)(implicit session: DBSession): List[Account] =
    val pattern = s"$kana%"
    sql"""
      SELECT * FROM "勘定科目マスタ"
      WHERE "勘定科目カナ" LIKE ${pattern}
      ORDER BY "勘定科目カナ", "勘定科目コード"
    """.map(fromResultSet).list.apply()

  /**
   * 勘定科目を更新
   */
  def update(account: Account)(implicit session: DBSession): Int =
    sql"""
      UPDATE "勘定科目マスタ"
      SET "勘定科目名" = ${account.accountName},
          "勘定科目種別" = ${account.accountType.value}::account_type,
          "残高" = ${account.balance},
          "BSPL区分" = ${account.bsplDistinction},
          "取引要素区分" = ${account.transactionElement},
          "費用区分" = ${account.expenseDistinction},
          "合計科目" = ${account.isSummaryAccount},
          "表示順序" = ${account.displayOrder},
          "集計対象" = ${account.isAggregationTarget},
          "勘定科目カナ" = ${account.accountKana},
          "課税取引コード" = ${account.taxCode},
          "更新日時" = CURRENT_TIMESTAMP
      WHERE "勘定科目コード" = ${account.accountCode}
    """.update.apply()

  /**
   * 残高を更新
   */
  def updateBalance(code: String, balance: BigDecimal)(implicit session: DBSession): Int =
    sql"""
      UPDATE "勘定科目マスタ"
      SET "残高" = ${balance},
          "更新日時" = CURRENT_TIMESTAMP
      WHERE "勘定科目コード" = ${code}
    """.update.apply()

  /**
   * 勘定科目を削除
   */
  def delete(code: String)(implicit session: DBSession): Int =
    sql"""
      DELETE FROM "勘定科目マスタ"
      WHERE "勘定科目コード" = ${code}
    """.update.apply()
