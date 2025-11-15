package com.example.domain

import scalikejdbc._

import java.time.LocalDateTime

/**
 * 在庫データのドメインモデル
 *
 * 倉庫ごと、商品ごと、ロット番号ごとに在庫数を管理
 *
 * @param whCode 倉庫コード
 * @param prodCode 商品コード
 * @param rotNo ロット番号
 * @param stockType 在庫区分（1:正常在庫、2:預かり在庫、3:不良在庫）
 * @param qualityType 品質区分（G:良品、B:不良品）
 * @param actual 実在庫数
 * @param valid 有効在庫数（実在庫 - 引当済）
 * @param lastDeliveryDate 最終出荷日
 * @param createDate 作成日時
 * @param creator 作成者
 * @param updateDate 更新日時
 * @param updater 更新者
 */
case class Stock(
  whCode: String,
  prodCode: String,
  rotNo: String,
  stockType: String = "1",
  qualityType: String = "G",
  actual: Int = 0,
  valid: Int = 0,
  lastDeliveryDate: Option[LocalDateTime] = None,
  createDate: LocalDateTime = LocalDateTime.now(),
  creator: String = "",
  updateDate: LocalDateTime = LocalDateTime.now(),
  updater: String = ""
)

object Stock extends SQLSyntaxSupport[Stock] {
  override val tableName = "在庫"
  override val columns = Seq(
    "倉庫コード", "商品コード", "ロット番号", "在庫区分", "品質区分",
    "実在庫数", "有効在庫数", "最終出荷日",
    "作成日時", "作成者名", "更新日時", "更新者名"
  )

  def apply(rs: WrappedResultSet): Stock = Stock(
    whCode = rs.string("倉庫コード"),
    prodCode = rs.string("商品コード"),
    rotNo = rs.string("ロット番号"),
    stockType = rs.string("在庫区分"),
    qualityType = rs.string("品質区分"),
    actual = rs.int("実在庫数"),
    valid = rs.int("有効在庫数"),
    lastDeliveryDate = rs.localDateTimeOpt("最終出荷日"),
    createDate = rs.localDateTime("作成日時"),
    creator = rs.string("作成者名"),
    updateDate = rs.localDateTime("更新日時"),
    updater = rs.string("更新者名")
  )
}
