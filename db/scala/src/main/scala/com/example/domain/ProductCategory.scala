package com.example.domain

import scalikejdbc._

import java.time.LocalDateTime

/**
 * 商品分類マスタのドメインモデル
 *
 * 商品を階層的に分類するためのマスタデータ
 *
 * @param categoryCode 商品分類コード
 * @param name 商品分類名
 * @param layer 階層レベル（1が最上位）
 * @param path 階層パス（例: /PC/PC01/）
 * @param lowestType 最下層区分（0:中間層、1:最下層）
 * @param createDate 作成日時
 * @param creator 作成者
 * @param updateDate 更新日時
 * @param updater 更新者
 */
case class ProductCategory(
  categoryCode: String,
  name: String,
  layer: Int = 0,
  path: String = "/",
  lowestType: Int = 0,
  createDate: LocalDateTime = LocalDateTime.now(),
  creator: String = "",
  updateDate: LocalDateTime = LocalDateTime.now(),
  updater: String = ""
)

object ProductCategory extends SQLSyntaxSupport[ProductCategory] {
  override val tableName = "商品分類マスタ"
  override val columns = Seq(
    "商品分類コード", "商品分類名", "商品分類階層", "商品分類パス", "最下層区分",
    "作成日時", "作成者名", "更新日時", "更新者名"
  )

  def apply(rs: WrappedResultSet): ProductCategory = ProductCategory(
    categoryCode = rs.string("商品分類コード"),
    name = rs.string("商品分類名"),
    layer = rs.int("商品分類階層"),
    path = rs.string("商品分類パス"),
    lowestType = rs.int("最下層区分"),
    createDate = rs.localDateTime("作成日時"),
    creator = rs.string("作成者名"),
    updateDate = rs.localDateTime("更新日時"),
    updater = rs.string("更新者名")
  )
}
