package com.example.domain

import scalikejdbc._

import java.time.LocalDateTime

/**
 * 代替商品のドメインモデル
 *
 * 商品の代替商品を管理する（多対多関係）
 *
 * @param prodCode 商品コード
 * @param altProdCode 代替商品コード
 * @param priority 優先順位（1が最優先）
 * @param createDate 作成日時
 * @param creator 作成者
 * @param updateDate 更新日時
 * @param updater 更新者
 */
case class AlternateProduct(
  prodCode: String,
  altProdCode: String,
  priority: Int = 1,
  createDate: LocalDateTime = LocalDateTime.now(),
  creator: String = "",
  updateDate: LocalDateTime = LocalDateTime.now(),
  updater: String = ""
)

object AlternateProduct extends SQLSyntaxSupport[AlternateProduct] {
  override val tableName = "代替商品"
  override val columns = Seq(
    "商品コード", "代替商品コード", "優先順位",
    "作成日時", "作成者名", "更新日時", "更新者名"
  )

  def apply(rs: WrappedResultSet): AlternateProduct = AlternateProduct(
    prodCode = rs.string("商品コード"),
    altProdCode = rs.string("代替商品コード"),
    priority = rs.int("優先順位"),
    createDate = rs.localDateTime("作成日時"),
    creator = rs.string("作成者名"),
    updateDate = rs.localDateTime("更新日時"),
    updater = rs.string("更新者名")
  )
}
