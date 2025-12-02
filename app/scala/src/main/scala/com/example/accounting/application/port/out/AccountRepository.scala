package com.example.accounting.application.port.out

import com.example.accounting.domain.account.{Account, AccountType}
import scalikejdbc.DBSession

/**
 * 勘定科目リポジトリ（Output Port）
 */
trait AccountRepository:

  /** 勘定科目を登録 */
  def insert(account: Account)(implicit session: DBSession): Int

  /** 勘定科目IDで検索 */
  def findById(id: Int)(implicit session: DBSession): Option[Account]

  /** 勘定科目コードで検索 */
  def findByCode(code: String)(implicit session: DBSession): Option[Account]

  /** すべての勘定科目を取得 */
  def findAll()(implicit session: DBSession): List[Account]

  /** 勘定科目種別で検索 */
  def findByType(accountType: AccountType)(implicit session: DBSession): List[Account]

  /** BSPL区分で検索 */
  def findByBsplDistinction(distinction: String)(implicit session: DBSession): List[Account]

  /** 集計対象の勘定科目を取得 */
  def findAggregationTargets()(implicit session: DBSession): List[Account]

  /** 合計科目を取得 */
  def findSummaryAccounts()(implicit session: DBSession): List[Account]

  /** 課税取引コードで検索 */
  def findByTaxCode(taxCode: String)(implicit session: DBSession): List[Account]

  /** 勘定科目カナで前方一致検索 */
  def findByKanaPrefix(kana: String)(implicit session: DBSession): List[Account]

  /** 勘定科目を更新 */
  def update(account: Account)(implicit session: DBSession): Int

  /** 残高を更新 */
  def updateBalance(code: String, balance: BigDecimal)(implicit session: DBSession): Int

  /** 勘定科目を削除 */
  def delete(code: String)(implicit session: DBSession): Int
