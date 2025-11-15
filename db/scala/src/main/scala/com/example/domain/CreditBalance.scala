package com.example.domain

import scalikejdbc._
import java.time.LocalDateTime

/**
 * 与信残高データのドメインモデル
 *
 * 取引先ごとの与信状況を保持する
 *
 * @param compCode 取引先コード
 * @param orderBalance 受注残高（未出荷の受注金額の合計）
 * @param recBalance 売掛残高（未入金の売上金額の合計）
 * @param payBalance 買掛残高（未払いの仕入金額の合計）
 * @param createDate 作成日時
 * @param creator 作成者名
 * @param updateDate 更新日時
 * @param updater 更新者名
 */
case class CreditBalance(
  compCode: String,
  orderBalance: Int,
  recBalance: Int,
  payBalance: Int,
  createDate: LocalDateTime = LocalDateTime.now(),
  creator: Option[String] = None,
  updateDate: LocalDateTime = LocalDateTime.now(),
  updater: Option[String] = None
)

object CreditBalance extends SQLSyntaxSupport[CreditBalance] {
  override val tableName = "与信残高"
  override val columns = Seq(
    "取引先コード", "受注残高", "売掛残高", "買掛残高",
    "作成日時", "作成者名", "更新日時", "更新者名"
  )

  def apply(rs: WrappedResultSet): CreditBalance = CreditBalance(
    compCode = rs.string("取引先コード"),
    orderBalance = rs.int("受注残高"),
    recBalance = rs.int("売掛残高"),
    payBalance = rs.int("買掛残高"),
    createDate = rs.timestamp("作成日時").toLocalDateTime,
    creator = rs.stringOpt("作成者名"),
    updateDate = rs.timestamp("更新日時").toLocalDateTime,
    updater = rs.stringOpt("更新者名")
  )
}
