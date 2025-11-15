package com.example.domain

import scalikejdbc._

import java.time.LocalDateTime

/**
 * 発注データのドメインモデル
 *
 * 仕入先への発注情報を保持する（ヘッダー）
 *
 * @param poNo 発注番号
 * @param poDate 発注日
 * @param deptCode 部門コード
 * @param startDate 開始日（部門の有効期間）
 * @param supCode 仕入先コード
 * @param supSubNo 仕入先枝番
 * @param empCode 担当社員コード
 * @param completeFlg 完了フラグ（0:未完了、1:完了）
 * @param createDate 作成日時
 * @param creator 作成者
 * @param updateDate 更新日時
 * @param updater 更新者
 */
case class PurchaseOrder(
  poNo: String,
  poDate: LocalDateTime,
  deptCode: String,
  startDate: LocalDateTime,
  supCode: String,
  supSubNo: Int,
  empCode: String,
  completeFlg: Int = 0,
  createDate: LocalDateTime = LocalDateTime.now(),
  creator: String = "",
  updateDate: LocalDateTime = LocalDateTime.now(),
  updater: String = ""
)

object PurchaseOrder extends SQLSyntaxSupport[PurchaseOrder] {
  override val tableName = "発注"
  override val columns = Seq(
    "発注番号", "発注日", "部門コード", "開始日", "仕入先コード", "仕入先枝番",
    "社員コード", "完了フラグ", "作成日時", "作成者名", "更新日時", "更新者名"
  )

  def apply(rs: WrappedResultSet): PurchaseOrder = PurchaseOrder(
    poNo = rs.string("発注番号"),
    poDate = rs.localDateTime("発注日"),
    deptCode = rs.string("部門コード"),
    startDate = rs.localDateTime("開始日"),
    supCode = rs.string("仕入先コード"),
    supSubNo = rs.int("仕入先枝番"),
    empCode = rs.string("社員コード"),
    completeFlg = rs.int("完了フラグ"),
    createDate = rs.localDateTime("作成日時"),
    creator = rs.string("作成者名"),
    updateDate = rs.localDateTime("更新日時"),
    updater = rs.string("更新者名")
  )
}

/**
 * 発注明細のドメインモデル
 *
 * 発注の明細情報を保持
 *
 * @param poNo 発注番号
 * @param poDetailNo 発注明細番号
 * @param prodCode 商品コード
 * @param qty 発注数量
 * @param price 発注単価
 * @param createDate 作成日時
 * @param creator 作成者
 * @param updateDate 更新日時
 * @param updater 更新者
 */
case class PurchaseOrderDetail(
  poNo: String,
  poDetailNo: Int,
  prodCode: String,
  qty: Int = 1,
  price: Int = 0,
  createDate: LocalDateTime = LocalDateTime.now(),
  creator: String = "",
  updateDate: LocalDateTime = LocalDateTime.now(),
  updater: String = ""
)

object PurchaseOrderDetail extends SQLSyntaxSupport[PurchaseOrderDetail] {
  override val tableName = "発注明細"
  override val columns = Seq(
    "発注番号", "発注明細番号", "商品コード", "数量", "単価",
    "作成日時", "作成者名", "更新日時", "更新者名"
  )

  def apply(rs: WrappedResultSet): PurchaseOrderDetail = PurchaseOrderDetail(
    poNo = rs.string("発注番号"),
    poDetailNo = rs.int("発注明細番号"),
    prodCode = rs.string("商品コード"),
    qty = rs.int("数量"),
    price = rs.int("単価"),
    createDate = rs.localDateTime("作成日時"),
    creator = rs.string("作成者名"),
    updateDate = rs.localDateTime("更新日時"),
    updater = rs.string("更新者名")
  )
}
