package com.example.accounting.domain.account

import scalikejdbc.*

import java.time.LocalDateTime

/**
 * 課税取引マスタのケースクラス
 */
case class TaxTransaction(
    taxCode: String,
    taxName: String,
    taxRate: BigDecimal,
    createdAt: LocalDateTime,
    updatedAt: LocalDateTime,
)

object TaxTransaction extends SQLSyntaxSupport[TaxTransaction]:
  override val tableName = "課税取引マスタ"
  override val columns = Seq(
    "課税取引コード",
    "課税取引名",
    "税率",
    "作成日時",
    "更新日時",
  )

  def apply(rs: WrappedResultSet): TaxTransaction = new TaxTransaction(
    taxCode = rs.string("課税取引コード"),
    taxName = rs.string("課税取引名"),
    taxRate = rs.bigDecimal("税率"),
    createdAt = rs.localDateTime("作成日時"),
    updatedAt = rs.localDateTime("更新日時"),
  )
